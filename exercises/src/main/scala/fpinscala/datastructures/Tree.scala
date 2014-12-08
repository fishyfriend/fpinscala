package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Ex. 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // Ex. 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Ex. 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // Ex. 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  // Ex. 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def size2[A](t: Tree[A]): Int = fold(t)(a=>1)(_ + _ + 1)
  def maximum2(t: Tree[Int]): Int = fold(t)(a=>a)(_ max _)
  def depth2[A](t: Tree[A]): Int = fold(t)(a=>0)((l,r)=>(l max r) + 1)
  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A,Tree[B]] (t) (a => Leaf(f(a))) ((l,r)=>Branch(l,r))
}
