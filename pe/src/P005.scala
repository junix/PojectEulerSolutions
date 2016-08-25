import Primes._
import scala.math._

object P005 {

  def ex(n:Long, base :Long):Long = {
    val e = (log(n) / log(base)).toLong
    pow(base,e).toLong
  }

  def solve(n :Long) = {
    def go(xs:List[Long]):Long = xs match {
      case Nil => 1
      case x :: xs1 => ex(n,x) * go(xs1)
    }
    val ps = primes.takeWhile(_<n).toList
    go(ps)
  }

  val euler = solve(20)

}
