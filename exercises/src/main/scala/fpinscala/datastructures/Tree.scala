// package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // def size[A](t: Tree[A]): Int = t match {
  //   case Leaf(_) => 1
  //   case Branch(l, r) => 1 + size(l) + size(r)
  // }

  // def maximum(t: Tree[Int]): Int = t match {
  //   case Leaf(v) => v
  //   case Branch(l, r) => maximum(l) max maximum(r)
  // }

  // def depth[A](t: Tree[A]): Int = t match {
  //   case Leaf(_) => 1
  //   case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  // }

  // def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
  //   case Leaf(v) => Leaf(f(v))
  //   case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  // }

  def fold[A,B](t: Tree[A])(f: Leaf[A] => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(Leaf(v))
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l,r) => 1 + l + r)

  def maximum(t: Tree[Int]): Int =
    fold(t)(_.value)((l,r) => l max r)

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l,r) => (l + 1) max (r + 1))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(l => Leaf(f(l.value)))((l,r) => Branch(l,r))
}

object Test {

  import Tree._

  def main(args: Array[String]): Unit = {
      val tree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(42)), Leaf(10)))
      println("Size should be 7")
      println("Size: ", size(tree))

      println("Maximum value in tree:")
      println(maximum(tree))

      println("Depth of tree:")
      println(depth(tree))

      println("Map tree")
      println(tree)
      println(map(tree)(_+13))

      println("Size tree")
      println(size(tree))
  }
}
