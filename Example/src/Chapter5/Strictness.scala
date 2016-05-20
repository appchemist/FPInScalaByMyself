package Chapter5

/**
  * Created by appchemist on 2016. 5. 18..
  */
sealed trait Stream[+A] {
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
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty)((a, b) => if (p(a)) Stream.cons(a, b) else Empty)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}