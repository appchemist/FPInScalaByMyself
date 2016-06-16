package Chapter6

import scala.List._;
import scala.::;

/**
  * Created by appchemist on 2016. 5. 24..
  */

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng2) = rng.nextInt

    if (i1 == Int.MinValue) (0, rng2)
    else if (i1 < 0) (-i1, rng2)
    else (i1, rng2)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng2) = nonNegativeInt(rng)

    (i1 / (Int.MaxValue.toDouble + 1), rng2)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng2) = nonNegativeInt(rng)
    val (d2, rng3) = double(rng2)

    ((i1, d2), rng3)
  }

  // 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)

    ((d, i), rng2)
  }

  // 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1, d2, d3), rng4)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) return (Nil, rng)
    else {
      val (i, rng2) = nonNegativeInt(rng)
      val (list, rng3) = ints(count - 1)(rng2)
      (i :: ints(count - 1)(rng2)._1, rng3)
    }
  }

  type Rand[+A] = RNG => (A, RNG)
//  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i=>i - i % 2)

  // 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble + 1)

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a, b), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((r, list) => map2(r, list)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (i, rng2) = f(rng)
    g(i)(rng2)
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
        val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThanViaFlatMap(n)
    }

  // 6.9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => unit(f(i)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))


  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((r, list) => map2ViaFlatMap(r, list)(_ :: _))

  def intsViaSequence2(count: Int): Rand[List[Int]] =
    sequence2(List.fill(count)(nonNegativeInt))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  // 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  // 6.10
  def map[B](f: A => B): State[S, B] =
    flatMap(i => State.unit(f(i)))

  // 6.10
  def map2[B, C](sb: State[S, B])(f: (A, B)=> C): State[S, C] =
    flatMap(i => sb.map(f(i, _)))
}

object State {
  type Rand[A] = State[RNG, A]

  // 6.10
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // 6.10
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((r, l) => r.map2(l)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
    case Coin :: t => Machine(false, candies, coins + 1).simulateMachine(t)
    case Turn :: t if locked => Machine(locked, candies, coins).simulateMachine(t)
    case Turn :: t => Machine(true, candies - 1, coins).simulateMachine(t)
    case Nil => State.unit((coins, candies))
  }
}