package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Ex. 5.1
  def toList: List[A] = foldRight[List[A]](Nil) { (a, as) => a::as }
  // stack-safe solution, see answers
  def toList_2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l.reverse
      case Cons(h, t) => go(t(), h()::l)
    }
    go(this, Nil)
  }

  // Ex. 5.2
  def take(n: Int): Stream[A] = if (n < 1) Empty else {
    this match {
      case Empty => Empty
      case Cons(h, t) => cons(h(), t().take(n-1))
  } }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: => Stream[A], m: Int): Stream[A] = if (m < 1) s else { s match {
      case Empty => Empty
      case Cons(h, t) => go(t(), n-1)
    } }
    go(this, n)
  }

  // Ex. 5.3
  // better solution from answers: put `Cons` case first, moving predicate check to a case filter
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), t() takeWhile p) else empty
  }

  // Ex. 5.4
  @annotation.tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && (t() forAll p)
  }
  // better (book solution):
  final def forAll_2(p: A => Boolean): Boolean =
    foldRight(true){ (a,b) => p (a) && b }

  // Ex. 5.5
  def takeWhileViaFoldRight(p:A=>Boolean):Stream[A] =
    foldRight[Stream[A]](Empty) { (a,b) => if (p(a)) cons(a,b) else Empty }

  // Ex. 5.6
  def headOption: Option[A] = foldRight[Option[A]](None) { (a, b) => Some(a) }

  // Ex. 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]){ (a, b) => cons(f(a), b)}
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]){ (a,b) => if (p(a)) cons(a,b) else b }
  def append[B>:A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]){ (a,b) => f(a) append b }

  // Ex. 5.14
  def startsWith[A](a: Stream[A]): Boolean =
    this.zipAll(a).takeWhile(_._2.isDefined).forAll { case (a,b) => a == b }

  // Ex. 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }
  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)){
    case (m, _) if m <= 0 => None
    case (_, Empty) => None
    case (m, Cons(h,t)) => Some((h(), (m-1, t())))
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h,t) =>
      val hh = h()
      if (p(hh)) Some((hh, t())) else None
    case _ => None
  }
  def zipWith[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, b){
      case (Cons(s,t), Cons(u,v)) => Some((f(s(),u()),(t(),v())))
      case _ => None
    }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this,s2){
    case (Empty, Empty) => None
    case (Empty, Cons(h,t)) => Some(((None, Some(h())), (Empty,t())))
    case (Cons(h,t), Empty) => Some(((Some(h()), None), (t(),Empty)))
    case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
  }

  // Before Ex. 5.14-15 -- using only methods implemented so far
  def hasSubsequence[B >: A](sub: Stream[B]): Boolean = {
    val supSubs = for {
        sup <- unfold(this) {
          case Empty => None
          case Cons(h, t) => Some(Cons(h, t) -> t())
        }
      } yield sup zipAll sub takeWhile (_._2.isDefined)
    supSubs exists { _.forAll { case (a,b) => a == b } }
  }
  // After Ex. 5.14-15 -- answer from book
  def hasSubsequence2[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  // Ex. 5.15
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case Cons(h,t) => Some(Cons(h,t) -> t())
  } append Stream(empty)

  // Ex. 5.16
  //
  // It is not possible to implement `scanRight` using `unfold` and have the
  // resulting stream be traversable in linear time. The inner function to
  // `unfold` can't "look ahead" to the later elements of the stream; but in the
  // result stream, the value of each element depends on the value of the
  // following element. Thus, getting the value of a single element requires
  // calculation of all later elements. Intermediate values can't be reused
  // since there's no way to give later elements access to earlier ones.
  //
  // One could maybe cheat by having the first iteration of the inner
  // function calculate the whole result in some manner that satisfies the
  // requirement of linear traversal, hold this result in the state,
  // and let the subsequent iterations of the function simply pop the pre-
  // calculated values off the state.
  //
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z)) { (a,b) =>
      lazy val b_ = b
      lazy val f_ = f(a, b_.headOption.get)
      cons(f_, b_)
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Ex. 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  // Book solution is more efficient:
  // def constant[A](a: A): Stream[A] = {
  //  lazy val tail: Stream[A] = Cons(() => a, () => tail)
  //  tail
  //}

  // Ex. 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // Ex. 5.10
  def fibs: Stream[Int] = {
    def go(a:Int, b:Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  // Ex. 5.11
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z) map { as => cons(as._1, unfold(as._2)(f)) } getOrElse empty[A]

  // Ex. 5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0,1)) { case (a,b) => Some((a, (b, a+b))) }
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(n, n+1))
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(())(_=>Some((a,())))
  def onesViaUnfold: Stream[Int] =
    unfold(())(_=>Some((1,())))
}
