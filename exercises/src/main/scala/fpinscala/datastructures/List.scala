package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  } // result = 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("can't take the tail of an empty list")
    // alternative: case Nil => Nil
    case Cons(h, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("can't set head of empty list")
    case Cons(hh, tt) => Cons(h, tt)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = 
    if (n > 0)
      l match {
        case Nil => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
    else l

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("can't take init of empty list")
    case Cons(h,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
  def init2[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(acc: List[A], l: List[A]): (List[A], List[A]) = l match {
      case Nil => throw new UnsupportedOperationException("can't take init of empty list")
      case Cons(_, Nil) => (acc, Nil)
      case Cons(h, t) => go(Cons(h,acc), t)
    }
    reverse(go(Nil, l)._1)
  }

  // Exercise 3.7
  // No, because if all the recursive calls to `foldRight` are made before `f` is called
  // even once. For a large list, one could preprocess the list by searching for an element
  // equal to 0, returning 0 if any is found, prior to performing the multiplication. This
  // doesn't avoid recursion but does avoid multiplication if a 0 is found. For some very
  // theoretical JVM implementation this might give better performance.
  def precheckedProduct(ns: List[Double]) = {
    def has0(l: List[Double]): Boolean = l match {
      case Nil => false
      case Cons(h, t) => if (h == 0.0) true else has0(t)
    }
    if (has0(ns)) 0.0
    else foldRight(ns, 1.0)(_ * _)
  }
  // More efficiently, one could implement `product` recursively in the style of `foldRight`
  // but with the zero check baked in, so that if a 0-value element is discovered the method
  // returns immediately instead of continuing with the recursion:
  def betterProduct(ns: List[Double]) = ns match {
    case Nil => 1.0
    case Cons(x, xs) =>
      if (x == 0.0) 0.0
      else x * product(xs)
  }

  // Exercise 3.8
  // The original list, regardless whether `Nil` or `Cons()`, is returned.
  // This shows that `foldRight`, when called with `z=Nil` and `f=Cons(_,_)`,
  // behaves as an identity function for the data constructors of `List`.
  // Put another way, the function:
  //   (l:List[A]) => foldRight(l, Nil:List[A])(Cons(_,_))
  // is an identity function.

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((a, n) => 1 + n)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sumViafoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productViafoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthViafoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((b, a) => Cons(a, b))

  // Exercise 3.13
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => bb => g(f(bb, a)))(z)
  def foldRightViafoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))
  def foldRightViaFoldLeft_2[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => bb => g(f(a, bb)))(z)

  // Exercise 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  // Exercise 3.15
  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(appendViaFoldRight)

  // Exercise 3.16
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a,b)=>Cons(a+1,b))

  // Exercise 3.17
  def toStrings(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a,b)=>Cons(a.toString,b))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B]) { (a,b) => Cons(f(a), b) }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A]) { (a,b) => if (f(a)) Cons(a, b) else b }
  def removeOdds(is: List[Int]) = filter(is)(_ % 2 == 0)

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B]) { (a,b) => append(f(a), b) }

  // Exercise 3.21
  def filterViaFlatMap[A](as:List[A])(f: A=>Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3.22
  def addElements(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => l2 // n.b. the text has `Nil` here
    case (_, Nil) => l1 // and here
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addElements(t1, t2))
  }

  // Exercise 3.23
  // Text solution is more general: ((List[A], List[B])(f: (A,B)=>C): List[C]
  // Like the previous exercise, the text solution reverts to `Nil` when one
  // list runs out of data. That's necessary since there is no result of type
  // `C` available when only one of the two input values is present. In my
  // solution, when one list runs out of data, the input value from the other
  // list is also taken to be the "result" value. That works for e.g. addition
  // and multiplication but might not be intuitive as a rule. So the text
  // solution is better for real-word applications.
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =
    (l1, l2) match {
      case (Nil, _) => l2
      case (_, Nil) => l1
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }
  def zipWith2[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith2(t1,t2)(f))
    }

  // Exercise 3.24
  // variation on the text solution
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith[A](a:List[A], b:List[A]):Boolean =
      length(b) == length(filter(zipWith2(a, b)(_ == _))(_ == true))
    sup match {
      case Nil => sub == Nil
      case Cons(_, t) => startsWith(sup, sub) || hasSubsequence(t, sub)
    }
  }
}
