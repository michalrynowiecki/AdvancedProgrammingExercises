// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (a, b) = rng.nextInt
    (math.abs(a), b)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (a, b) = rng.nextInt
    val a2 = math.abs(a.toDouble)/Int.MaxValue.toDouble
    (a2, b)

  // Exercise 3
  
  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ( (Int, Double), RNG) = 
    val (a, b) = nonNegativeInt(rng)
    val (a2, b2) = double(b)
    ( (a, a2), b2)

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ( (Double, Int), RNG) = 
    val (a, b) = nonNegativeInt(rng)
    val (a2, b2) = double(b)
    ( (a2, a), b2)

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): (List[Int], RNG) = 
    def aux(size: Int)(l: List[Int])(rng: RNG) : (List[Int], RNG) =
      val (a,b) = rng.nextInt
      size match
        case 0  => (l, b)
        case _  => aux(size-1)(a::l)(b)
    aux(size)(List.empty)(rng)


  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    map(double) {i => i.toDouble}

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng0 =>
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)


  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit(List[A]()))((ra, acc) => map2(ra, acc)((ao, accList) => ao :: accList))

  def ints2(size: Int): Rand[List[Int]] = {
  // Create a list of `Rand[Int]` of the specified size
    val randInts: List[Rand[Int]] = List.fill(size)(int)
    // Use `sequence` to combine them into a single `Rand[List[Int]]`
    sequence(randInts)
}

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng0 =>
      val (a, rng1) = f(rng0)
      g(a)(rng1)

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    flatMap(nonNegativeInt) ( n => 
      val a = n % bound
      if (n+ (bound - 1) - a >= 0) unit(a)
      else nonNegativeLessThan(bound)
    )

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    State ( s =>
      val (a, s1) = this.run(s)
      f(a).run(s1)
    )

  def map[B](f: A => B): State[S, B] = 
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    flatMap(a => sb.map(b => f(a, b)))


object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(State.unit[S, List[A]](List.empty)) ( (sa, acc) =>
      sa.map2(acc)(_::_)
      )

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] = 
    val (a, nextState) = s.run(initial)
    LazyList.cons(a, stateToLazyList(s)(nextState))

  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 
    stateToLazyList(State[RNG, Int](_.nextInt))(rng)

  lazy val tenStrictInts: List[Int] = 
    lazyInts(RNG.SimpleRNG(42)).take(10).toList

end State
