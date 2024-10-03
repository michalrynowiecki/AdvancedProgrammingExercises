// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n*n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))
    println(square(4))

end MyModule

// Exercise 2 requires no programming

// Exercise 3

def fib(n: Int): Int =
  def go(prev:Int, prevprev:Int, m:Int) : Int =
    if m == n-1 then (prev)
    else go(prevprev, prev+prevprev, m+1)
  go(0, 1, 0)

// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
  def go(n:Int): Boolean =
    if as.length == 0 then true else
    
    if n==(as.length-1) then true else
    
    if ordered(as(n), as(n+1)) then go(n+1)
    else false
  go(0)

// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  A => (B => f(A, B))

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  curry(isSorted)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (A, B) => f(A)(B)

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  uncurry(isSortedCurried)

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  A => f(g(A))
