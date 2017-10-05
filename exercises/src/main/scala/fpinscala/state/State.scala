package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Ex. 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n > Int.MinValue) (Math.abs(n), rng2) else (0, rng2)
  }

  // Ex. 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (-(n.toDouble / Int.MinValue), rng2)
  }

  // Ex. 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), rng2) = intDouble(rng)
    ((d,i), rng2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    var r = rng
    val ds = new Array[Double](3)
    for (i <- 0 to 3) {
      val (d, r2) = double(r)
      ds(i) = d
      r = r2
    }
    ((ds(0), ds(1), ds(2)), r)
  }

  // Ex. 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (n, rng2) = rng.nextInt
      val (ns, rng3) = ints(count - 1)(rng2)
      (n :: ns, rng3)
    }
  }

  // Ex. 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(n => -(n.toDouble / Int.MinValue))

  // Ex. 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  // Ex. 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    val init: Rand[List[A]] = unit(List.empty[A])
    val outer = fs.foldRight(init) { (f, acc) => map2(f, acc)(_ :: _) }
    outer(rng)
  }
  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)((r: RNG) => r.nextInt)).apply(rng)

  // Ex. 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    val (b, rng3) = g(a)(rng2)
    (b, rng3)
  }
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  // Ex. 6.9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a,b)
    } }
}

case class State[S,+A](run: S => (A, S)) {
  // Ex. 6.10
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

   def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
     for { a <- this; b <- sb } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  // Ex. 6.10 continued
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    val init: State[S, List[A]] = unit(List.empty[A])
    fs.foldRight(init) { (f, acc) =>
      f.map2(acc)(_ :: _)
    }
  }

  // Ex. 6.11

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def transition(input: Input): State[Machine, Unit] = modify { s =>
    (input, s) match {
      case (Coin, Machine(true, ca, co)) if ca > 0 => Machine(false, ca, co + 1)
      case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)
      case _ => s
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map transition)
    s <- get
  } yield (s.coins, s.candies)
}
