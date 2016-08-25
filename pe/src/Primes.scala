import scala.collection.immutable.Stream._
import scala.collection.immutable.Stream
import scala.BigInt

object Primes {
  def bigInts(n: BigInt): Stream[BigInt] = n #:: bigInts(n + 1)

  def longs(n: Long): Stream[Long] = n #:: longs(n + 1)

  def sieve: Stream[Long] = filter(longs(2))

  def filter(s: Stream[Long]): Stream[Long] = s match {
    case (hd #:: tl) => hd #:: filter(tl.filter(_ % hd != 0))
  }

  def primes: Stream[Long] = 2 #:: prime3

  // performance: avoid redundant divide by two, so this starts at 3
  private val prime3: Stream[Long] = {
    def next(i: Long): Stream[Long] =
      if (prime(i))
        i #:: next(i + 2)
      else
        next(i + 2) // tail
    3 #:: next(5)
  }

  private def prime(i: Long) =
    prime3.takeWhile(math.sqrt(i).>= _).forall {
      i % _ != 0
    }

  def factorise(i: Long) = {

    def go(v: Long, s: Stream[Long]): List[Long] = (v, s) match {
      case (1, _) => Nil
      case (_, Stream()) => List(v)
      case (_, d #:: s1) if v % d == 0 => d :: go(v / d, s)
      case _ => go(v, s.tail)
    }

    val r = math.sqrt(i)
    val xs = primes.takeWhile(_ <= r)
    go(i, xs)
      .groupBy(x => x)
      .toSeq
      .map { case (k, v) => (k, v.length) }
      .sortBy(_._1)
  }

}
