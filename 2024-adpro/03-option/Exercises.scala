// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int =
    if this.x < that.x || (this.x == that.x && this.y < that.y) then (-1)
    else if (this.x == that.x && this.y == that.y) then 0 else 1

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro.option.*
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int =
    t match
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)

  // Exercise 3

  def maximum(t: Tree[Int]): Int = 
    def getVal(t:Tree[Int]): Int =
      t match
        case Leaf(x) => x
        case Branch(left, right) => getVal(left)
    
    val n = getVal(t)
    
    def aux(n: Int, t:Tree[Int]): Int =
      t match
        case Leaf(x) => x.max(n)
        case Branch(left, right) => aux(n, left).max(aux(n, right))
    aux(n, t)
    

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    t match
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    

  // Exercise 5
  
  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = 
    t match
      case Leaf(x) => g(x)
      case Branch(left, right) => f(fold(left)(f)(g), fold(right)(f)(g))
    

  def size1[A](t: Tree[A]): Int = fold(t)((x:Int, y:Int) => x + y + 1)((a) => 1)

  def maximum1(t: Tree[Int]): Int = fold[Int,Int](t)((x, y) => if x > y then x else y)((a) => a)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    fold(t)((x: Tree[B], y: Tree[B]) => Branch(x,y))(x=> Leaf(f(x)))




enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] =
    this match
      case Some(value)  => Some(f(value))
      case None         => None
    

  def getOrElse[B >: A] (default: => B): B = 
    this match
      case Some(value)  => value
      case None         => default     
    

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match
      case Some(value)    => f(value)
      case None           => None


  def filter(p: A => Boolean): Option[A] =
    this match
      case Some(value)    => if p(value) then Some(value) else None
      case None           => None
    

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)




object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    for {
      a <- ao
      b <- bo
    } yield f(a, b)

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight (Some(List())) ((ao, acc) => map2(ao, acc) ((ao,acc) => ao::acc))

  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List())) ((ao, acc) => map2(f(ao), acc) ((ao,acc) => ao::acc))

end Option



// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] =
  headOption(lst).map((x) => x._2)

def headGrade1(lst: List[(String,Int)]): Option[Int] =
  for {
    a <- headOption(lst)
  } yield a._2

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m =>
    Some(xs.map(x => math.pow(x - m, 2)).sum / xs.length))

// Scroll up, to the Option object for Exercise 9
def variance2(xs: Seq[Double]): Option[Double] =
  for {
    m <- mean(xs)
  } yield xs.map(x => math.pow(x-m, 2)).sum / xs.length