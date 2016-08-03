import scala.annotation.tailrec

object P173 {

  def lenSeq(n: Int): Stream[Int] = {
    def lenOf(n: Int) = if (n == 1) 1 else (n - 1) * 4

    lenOf(n) #:: lenSeq(n + 2)
  }

  @tailrec
  def tryGen(n: Int, xs: Stream[Int], cont: Int, acc: Int): Int = {

    @tailrec
    def carve(n: Int, xs: Stream[Int], acc: Int): Int = xs match {
      case v #:: _ if v > n => acc
      case v #:: _ if v == n => acc + 1
      case v #:: t => carve(n - v, t, acc + 1)
    }

    xs match {
      case v #:: _ if v > n => acc
      case v #:: _ if v == n => acc + 1
      case v #:: t => tryGen(n, t, 0, carve(n, xs, cont) + acc)
    }
  }

  def p173(n: Int) = (3 to 4).map(x => lenSeq(x)).map(s => tryGen(n, s, 0, 0)).sum

  def main(args: Array[String]) = {
    println(p173(1000000))
  }
}