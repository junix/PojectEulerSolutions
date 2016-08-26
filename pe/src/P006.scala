object P006 {

  val solve = {
    val seq = 1 to 100
    val s0 = seq.map{x=>x*x}.sum
    val s1 = seq.sum
    s1 * s1 - s0
  }

}
