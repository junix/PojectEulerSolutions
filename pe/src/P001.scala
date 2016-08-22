/*
If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*/

import scala.collection.immutable.Stream
import scala.annotation.tailrec

object P001 extends App {
}

object PE001 {
  def mod35(x :Int) : Boolean = x % 3 == 0 || x % 5 == 0

  def solve = {
    val m = for ( i <- 1 until 1000 if mod35(i)) yield i
    m.sum
  }

  def solve2 = {
    val s0 = 3 until (1000, 3)
    val s1 = 5 until (1000, 5)
    (s0 union s1).toSet.sum
  }

  def ints(n :Int) :Stream[Int] = n #:: ints(n+1)

  def solve3 = ints(1)
              .takeWhile(_<1000)
              .filter(mod35)
              .sum
}


