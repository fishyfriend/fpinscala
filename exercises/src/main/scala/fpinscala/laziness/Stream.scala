package fpinscala.laziness

import Stream._
trait Stream[+A] {

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

  // Ex. 5.5
  def takeWhile2(p:A=>Boolean):Stream[A] = foldRight[Stream[A]](Empty) { (a,b) =>
    if (p(a)) cons(a,b) else Empty
  }

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
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
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
