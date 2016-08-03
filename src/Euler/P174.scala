import scala.annotation.tailrec
import scala.collection.mutable.HashMap

object P174 {

  def lenSeq(n: Int): Stream[Int] = {
    def lenOf(n: Int) = if (n == 1) 1 else (n - 1) * 4

    lenOf(n) #:: lenSeq(n + 2)
  }

  def update(d: HashMap[Int, Int], k: Int) = {
    d(k) = d.getOrElse(k, 0) + 1
    d
  }

  @tailrec
  def carve(n: Int, xs: Stream[Int], acc: Int, d: HashMap[Int, Int]): HashMap[Int, Int] = xs match {
    case v #:: _ if n < (v + acc) => d
    case v #:: _ if n == (v + acc) => update(d, n)
    case v #:: t => {
      val nacc = v + acc
      carve(n, t, nacc, update(d, nacc))
    }
  }

  @tailrec
  def tryGen(n: Int, xs: Stream[Int], d: HashMap[Int, Int]): HashMap[Int, Int] = {

    xs match {
      case v #:: _ if v > n => d
      case v #:: _ if v == n => update(d, v)
      case v #:: t => tryGen(n, t, carve(n, t, v, update(d, v)))
    }
  }

  def p174(n: Int, l:Int) = {
    val d = tryGen(n,
      lenSeq(4),
      tryGen(n,
        lenSeq(3),
        new HashMap[Int, Int]()))
    val d1 = d.filter(_._2 == l)
    d1.size
  }

  def main(args: Array[String]) = {
    println((1 to 10).map(p174(1000000, _)).sum)
  }
}