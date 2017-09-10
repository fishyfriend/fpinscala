package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match { case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
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
  def toList2: List[A] = {
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
  final def forAll2(p: A => Boolean): Boolean = foldRight(true){ (a,b) => p(a) && b }

  // Ex. 5.5
  def takeWhile2(p:A=>Boolean):Stream[A] =
    foldRight[Stream[A]](Empty) { (a,b) => if (p(a)) cons(a,b) else Empty }

  // Ex. 5.6
  def headOption: Option[A] = foldRight[Option[A]](None) { (a, b) => Some(a) }

  // Ex. 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]){ (a, b) => cons(f(a), b)}
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]){ (a,b) => if (p(a)) cons(a,b) else b }
  def append[B>:A](b: => Stream[B]): Stream[B] =
    foldRight(b)(cons)
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]){ (a,b) => f(a) append b }

  // Ex. 5.14
  def startsWith[B](s: Stream[B]): Boolean = this zipAll s forAll {
    case (Some(a), Some(b)) if a == b => true
    case (Some(a), None) => true
    case _ => false
  }
  // book solution is better: it cleanly separates the equality check from iterating to
  // the end of the 2nd stream
  def startsWith2[B](s: Stream[B]): Boolean = this.zipAll(s)
                                                  .takeWhile { case (a,b) => b.isDefined }
                                                  .forAll { case (a,b) => a==b }

  // Ex. 5.13
  def map2[B](f: A => B): Stream[B] = unfold[B,Stream[A]](this){
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }
  def take2(n: Int): Stream[A] = unfold((n, this)){
    case (m, _) if m <= 0 => None
    case (_, Empty) => None
    case (m, Cons(h,t)) => Some((h(), (m-1, t())))
  }
  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this){
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

  // Ex. 5.15
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s@Cons(_,t) => Some(s -> t())
  }

  // Ex. 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ??? //TODO
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
  // Book solution is marginally more efficient
  //def constant[A](a: A): Stream[A] = {
    //lazy val tail: Stream[A] = Cons(() => a, () => tail) 
    //tail
  //}

  // Ex. 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // Ex. 5.10
  def fibs: Stream[Int] = {
    def go(a:Int, b:Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  // Ex. 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((aa, ss)) => cons(aa, unfold(ss)(f))
      case None => Empty
    }
  }

  // Ex. 5.12
  def fibs2: Stream[Int] = unfold[Int, (Int,Int)]((0,1)) { case (a,b) => Some((a, (b, a+b))) }
  def from2(n: Int): Stream[Int] = unfold[Int,Int](n)(x => Some((n,n+1)))
  def constant2[A](a: A): Stream[A] = unfold[A,A](a)(x=>Some((x,x)))
  def ones2: Stream[Int] = unfold[Int,Int](1)(x=>Some((x,x)))
}
