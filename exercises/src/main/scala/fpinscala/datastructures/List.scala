package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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
    case Cons(h, t) =>
      def go(hh: A, tt: List[A]): List[A] = tt match {
        case Nil => Nil
        case Cons(hhh, ttt) => Cons(hh, go(hhh, ttt))
      }
      go(h, t)
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
  // skip

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((a, n) => 1 + n)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((b, a) => Cons(a, b))

  // Exercise 3.13
  def foldLeft2[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => bb => g(f(bb, a)))(z)
  def foldRight2[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))
  def foldRight3[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => bb => g(f(a, bb)))(z)

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  // Exercise 3.15
  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A]) { (a, b) =>
      foldRight(a, b)(Cons(_, _))
    }

  // Exercise 3.16
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a,b)=>Cons(a+1,b))

  // Exercise 3.17
  def toS(l: List[Double]): List[String] =
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
  def filter2[A](as:List[A])(f: A=>Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3.22
  def addEls(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => l2
    case (_, Nil) => l1
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addEls(t1, t2))
  }

  // Exercise 3.23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =
    (l1, l2) match {
      case (Nil, _) => l2
      case (_, Nil) => l1
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val ls = foldRight(sup, Nil:List[List[A]]) { (a, ls) =>
      Cons(List(a), map(ls)(Cons(a, _)))
    }
    foldLeft(ls, false) { (b, l) =>
      if (b) true
      else Nil == foldLeft(l, sub) { (toM, a) =>
        toM match {
          case Nil => Nil
          case Cons(x, xs) => if (a == x) xs else sub
    } } } }
}
