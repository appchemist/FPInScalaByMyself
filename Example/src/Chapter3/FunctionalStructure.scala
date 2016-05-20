package Chapter3

import scala.annotation.tailrec

/**
  * Created by appchemist on 2016. 5. 19..
  */


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as isEmpty) Nil
    else Cons(as head, apply(as tail: _*))

  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  // 3.3
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => Cons(head, t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n == 1 => t
    case Cons(_, t) if n > 1 => drop(t, n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Cons(h, t) => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (t == Nil) Nil else Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => b + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 3.11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((a, b) => a + 1)

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((a, b) => Cons(b, a))


  // 3.13
  // TODO : 답안 중 reverse를 사용하지 않는 부분 확인하자
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  // 3.14
  def append[A](as:List[A], bs:List[A]): List[A] =
    foldRightViaFoldLeft(as, bs)(Cons(_, _))

  // 3.15
  def flat[A](as:List[List[A]]): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A])(append(_, _))

  // 3.16
  def add1(as:List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // 3.17
  def toString(as:List[Double]): List[String] =
    foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  // 3.18
  // TODO : 답안에 다른 구현들을 확인하자
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if(f(a)) Cons(a, b) else b)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, b) => append(f(a), b))

  // 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def add(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(asH, asT), Cons(bsH, bsT)) => Cons(asH + bsH, add(asT, bsT))
  }

  // 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(asH, asT), Cons(bsH, bsT)) => Cons(f(asH, bsH), zipWith(asT, bsT)(f))
  }

  // TODO : 3.24
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](ts: Tree[A]): Int = ts match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // 3.26
  def maximum(ts: Tree[Int]): Int = ts match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // 3.27
  def depth[A](ts: Tree[A]): Int = ts match {
    case Leaf(v) => 1
    case Branch(l, r) => depth(l) max depth(r) + 1
  }

  // 3.28
  def map[A, B](ts: Tree[A])(f: A => B): Tree[B] = ts match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // TODO : 3.29
}