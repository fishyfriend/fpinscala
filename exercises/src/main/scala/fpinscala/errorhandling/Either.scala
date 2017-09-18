package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

  // Ex. 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for (aa <- this; bb <- b) yield f(aa, bb)
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // Ex. 4.7
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f(_))) { (a, as) => a :: as }
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

/* Ex. 4.8
 * The objective is to report two separate errors in the case where both arguments to mkPerson are invalid.
 * The possible solutions given are (a) change map2, (b) change the signature of mkPerson, (c) create a new
 * data type that is better able to report multiple errors.
 * Since Either[] captures only a single exception, (a) and (b) would necessitate somehow reporting two
 * errors with a single exception object. That's unwieldy; it would require a new `MultiException` type in
 * order to preserve the types of the original two exceptions, and would furthermore require the caller to
 * add additional logic to make pattern matching on the exceptions work correctly.
 * As a better alternative one could (c) create a new type EitherPlusPlus which accumulates errors in a
 * list. The class definition and constructors could look like this:
 *
 *     sealed trait EitherPlusPlus[+E,+A]
 *     case class LeftPlusPlus[+E](get: List[E]) extends EitherPlusPlus[E, Nothing]
 *     case class RightPlusPlus[+A](get: A) extends EitherPlusPlus[Nothing, A]
 *
 * orElse, if called on a LeftPlusPlus, would have to preserve the object's
 * existing error list. This only matters when the b argument is a LeftPlusPlus.
 * Rather than returning the b argument verbatim, orElse would return a new LeftPlusPlus
 * with the "inherited" errors appended to the error list from the b argument.
 * sequence and traverse might continue to behave as they do now (meaning the first
 * error encountered is the one reported) or they could "look at" the entire list and return
 * a list of all errors encountered during the traversal.
 */
