package com.kensai.aoc

object Day09 {

  //TODO: clean everything later
  
  def doCompute(n: Int, input: List[Long]): Option[Long] = {
    val toto = input
      .combinations(2)
      .find(_.sum == input(n))
    if (toto.isDefined) None
    else Some(input(n))
  }

  def compute(n: Int, inputs: List[String]): Long = {
    val values = inputs.map(_.trim).filterNot(_.isEmpty).map(_.toLong)
    (0 until inputs.size - n).map(i => values.slice(i, i + n + 1 )).flatMap(s => {
      doCompute(n, s)
    }).head
  }

  def compute2(n: Int, inputs: List[String]): Long = {
    val values = inputs.map(_.trim).filterNot(_.isEmpty).map(_.toLong)
    (0 until inputs.size - n).map(i => values.slice(i, i + n + 1 )).flatMap(s => {
      val r = doCompute(n, s)
      r.map(pbValue => {
        println(s"s[$s]")
        (0 until inputs.size - n).map(doCompute2(_, pbValue, values)).filter(_ > 0).head
      })
    }).head
  }

  def doCompute2(currentIndex: Int, pbValue: Long, values: List[Long]): Long = {
    if (currentIndex + 1 >= values.size) return -1

    var tmpSum = values(currentIndex) + values(currentIndex + 1)
    var index = currentIndex + 1
    while(values(index) < pbValue) {
      val slice = values.slice(currentIndex, index)
//      println(s"pbValue[$pbValue] slice[$slice] index[$index] tmpSum[$tmpSum]")
      if (tmpSum == pbValue) return slice.min + slice.max
      index = index + 1
      tmpSum = tmpSum + values(index)
    }

    -1
  }


}
