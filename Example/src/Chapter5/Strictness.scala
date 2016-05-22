package Chapter5

import Chapter4._

/**
  * Created by appchemist on 2016. 5. 18..
  */
sealed trait Stream[+A] {
  def empty[A]: Stream[A] = Empty

  // 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // 5.2
  def take(n:Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n -1))
    case Cons(h, t) if n == 1=> Stream.cons(h(), Empty)
  }

  // 5.2
  def drop(n:Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if (n > 0)  => t().drop(n - 1)
    case Cons(h, t) => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, _) if (!p(h())) => Empty
    case Cons(h, t) => Stream.cons(h(), t().takeWhile(p))
  }


  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))//t().foldRight(f(h(), z))(f)
    case _ => z
  }

  // 5.4
  def foldAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  // 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  // 5.7
  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  // 5.7
  def append[B>:A](other: Stream[B]): Stream[B] =
    foldRight(other)(Stream.cons(_, _))

  // 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty: Stream[A] else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, Stream.constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, Stream.from(n + 1))

  // 5.10
  def fibs: Stream[Int] = {
    def go(prev: Int, acc: => Int): Stream[Int] =
      Stream.cons(acc, go(acc, prev + acc))
    Stream.cons(0, go(0, 1))
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty: Stream[A]
  }

  // 5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(s => s match { case (prev, acc) => Some(prev, (acc, acc+prev)) case _ => None })

  // 5.12
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s+1))

  // 5.12
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  // 5.12
  def ones: Stream[Int] =
    constantViaUnfold(1)
}