package com.kensai.aoc

object Day13 {

  def parse(inputs: List[String]): (Long, Set[Long]) = {
    val rows = inputs.map(_.trim).filterNot(_.isEmpty)
    val bus = rows.tail.head.split(",").map(_.trim).filterNot(_ == "x").map(_.toLong).toSet
    (rows.head.toLong, bus)
  }

  def doCompute(input: (Long, Set[Long])): Option[(Long, Long)] = {
    val currentTimestamp = input._1
    val buses = input._2
    val busesMin = buses.min
    (currentTimestamp to (currentTimestamp + busesMin))
      .flatMap(t => buses.map(b => (t, b, t % b)))
      .find(_._3 == 0)
      .map(tupple => (tupple._1, tupple._2))
  }

  def compute(inputs: List[String]): Long = {
    val input = parse(inputs)
    println(s"$input")
    val (timestamp, bus) = doCompute(input).head
    println(s"inputTimestamp[${input._1}] timestamp[$timestamp] bus[$bus] diff[${timestamp - input._1}]")
    (timestamp - input._1) * bus
  }

  def parse2(inputs: List[String]): Map[Long, Long] = {
    inputs
      .map(_.trim)
      .filterNot(_.isEmpty)
      .tail
      .head
      .split(",")
      .map(_.trim)
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map{case (bus, index) => bus.toLong -> index.toLong}
      .toMap
  }

//  def compute2(inputs: List[String]): Long = {
//    val input = parse2(inputs)
//    doCompute2(input)
//  }

//  def doCompute2(input: Map[Long, Long]): Long = {
//    println(s"$input")
//    val maxTimestamp = input.keySet.product
//    println(s"maxTimestamp[$maxTimestamp}")
//
//    val (tmpMaxBus, relativeOffset) = input.maxBy(_._1)
//    val maxBus = tmpMaxBus.toLong
//    println(s"maxBus[$maxBus]")
//
//    var i = maxBus.toLong
//    while (i < maxTimestamp) {
//      if((i/maxBus) % 1000L ==0 ) println(s"$i -> ${i/maxBus}")
//      if (isTimestampValid2(i, relativeOffset, input))
//        return i - relativeOffset
//      i = i + maxBus
//    }
//
//    0L
//  }

  def isTimestampValid2(timestamp: Long, relativeOffset: Long, input: Map[Long, Long]): Boolean = {
    input
      .map{case (bus, offset) => (timestamp - (relativeOffset  - offset.toLong)) % bus.toLong}
      .forall(_ == 0L)
  }

  def isTimestampValid(t: Long, input: Map[Long, Long]): Boolean =
    input
      .map{case (bus, offset) => (t + offset.toLong) % bus.toLong}
      .forall(_ == 0L)

  def ei(ni: Long, n: Long): Long = {
    val niPrime = n / ni
    println(s"ni[$ni] n[$n] niPrime[$niPrime]")
    (0 to Int.MaxValue)
      .find(i => (i.toLong * niPrime) % ni  == 1)
      .get * niPrime
  }

  def chineseRemainderTheorem(input: Map[Long, Long]): Long = {
    val n = input.keySet.foldLeft(1L)(_ * _)
    val result = input
      .map{case (ni, rest) => {
        val e_i = ei(ni, n)
        val r = e_i * rest
        r
      }}
      .foldLeft(0L)(_ + _)
    result % n
  }

  def compute2(input: Map[Long, Long]): Long = {
    // Adapt to Chinese Remainder Theorem
    val tmp = input.map{case (bus, i) => bus -> (-i % bus + bus) % bus}
    chineseRemainderTheorem(tmp)
  }
}
