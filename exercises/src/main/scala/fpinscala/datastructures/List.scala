// package fpinscala.datastructures

sealed trait List[+A]
// `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing]
// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => l
      case Cons(x, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if (f(h)) dropWhile(t)(f)
      else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    def go[A](a: List[A], b: List[A]): List[A] = a match {
      case Nil => b
      case Cons(x, Nil) => b
      case Cons(x, xs) => go(xs, Cons(x ,b))
    }
    go(l, Nil)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def length[A](l: List[A]): Int =
    foldLeft(l, 0)((b, a) => b + 1)

  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)((b, a) => a + b)

  def product3(l: List[Int]): Int =
    foldLeft(l, 1)((b, a) => a * b)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, b) => Cons(a, b))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    reverse(foldLeft(l, Nil: List[B])((b, a) => Cons(f(a), b)))

  def flatten[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])((a, b) => append(a, b))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    def go(l: List[A], e: List[A]): List[A] = l match {
      case Nil => e
      case Cons(h, t) => if (f(h)) go(t, Cons(h, e)) else go(t, e)
    }

    reverse(go(l, Nil: List[A]))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean) =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zipWith[A,B,C](l: List[A], g: List[B], f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys), f)
  }
}


object Main {
  import List._

  def main(args: Array[String]): Unit = {
      val l = List(1,2,3,4,5,6,7)
      val r = List(8,9,42)
      val ll = List(l, r)
      val ldouble = List(1.0,2.0,3.0,4.0,5.0,6.0,7.0)

      val ld = drop(l, 3)
      println(l)
      println("Drop 3 from l")
      println(ld)
      println("Set head of l to 42")
      val lh = setHead(l, 42)
      println(lh)

      println("Append l and ld")
      println(append(l, ld))

      println("Drop while < 4")
      println(dropWhile(l)(_ < 4))

      println("init of l")
      println(init(l))

      println("Length of l")
      println(length(l))

      println("foldLeft")
      println(foldLeft(l, 0)((b, a) => b + a))

      println("Reverse of l")
      println(reverse(l))

      println("Sum of l")
      println(sum3(l))


      println("Product of l")
      println(product3(l))

      println("foldRight")
      println(foldRight(l, 0)((a, b) => a + b))

      println("append")
      println(append(l, r))

      println("Flatten")
      println(flatten(ll))
      println(ll)

      println("Map _^2 to l")
      println(map(l)(x => x*x))

      println("Filter odds")
      println(filter(l)(_ % 2 == 0))

      println("flatMap")
      println(flatMap(l)(x => List(x, x)))

      println("filterViaFlatMap")
      println(filterViaFlatMap(l)(_ % 2 == 0))
  }
}
