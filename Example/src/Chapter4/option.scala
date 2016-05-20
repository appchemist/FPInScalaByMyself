package Chapter4

/**
  * Created by appchemist on 2016. 5. 12..
  */
//import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  // 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  // 4.1
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  // 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  // 4.1
  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    //    val mean = (xs sum / xs length)
    //    Option[Double].flatMap(xs foldLeft(0)(_ + (math.pow(_ - mean, 2))))
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  }

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aValue => b flatMap (bValue => Some(f(aValue, bValue))))

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head::tail => map2(f(head), traverse(tail)(f))(_ :: _)
  }

  //4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    Try(a map({ case None => throw new IllegalStateException("Exception thrown"); case Some(value) => value }))

  //4.5
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def insuraceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    1.0
  }
  def parseInsuranceRateQuote(age: String,
                              numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuraceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}