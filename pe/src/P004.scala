object P004 {

  def palind(i: Long): Boolean = {
    val str = i.toString
    val rstr = str.reverse
    str == rstr
  }

  def solve = {
    val xs = for {i <- 100 to 999
                  j <- 100 to 999
                  if (palind(i * j))} yield i * j
    xs.max
  }

}
