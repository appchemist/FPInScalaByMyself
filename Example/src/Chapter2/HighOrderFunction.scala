package Chapter2

import scala.annotation.tailrec

/**
  * Created by appchemist on 2016. 5. 19..
  */
object HighOrderFunction {
  // 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(index: Int, previousValue: Int, accumulator: Int): Int = {
      if (index >= n)
        accumulator;
      else
        go(index+1, accumulator, accumulator + previousValue);
    }
    if (n == 0) 0
    else if (n == 1) 1
    else go(1, 0, 1)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) true
    else if (!ordered(as.head, as.tail.head)) false
    else isSorted(as.tail, ordered)
  }

  def partial1[A, B, C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    A => B => f(A, B)

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}