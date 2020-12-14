package com.kensai.aoc

object Day13 {

  def parse(inputs: List[String]): (Long, Set[Long]) = {
    val rows = inputs.map(_.trim).filterNot(_.isEmpty)
    val currentTimestamp = rows.head.toLong
    val bus = rows.tail.head.split(",").map(_.trim).filterNot(_ == "x").map(_.toLong).toSet
    (currentTimestamp, bus)
  }

  def computeEarliestBus(input: (Long, Set[Long])): Option[(Long, Long)] = {
    val currentTimestamp = input._1
    val buses = input._2
    val minBus = buses.min
    (currentTimestamp to (currentTimestamp + minBus))
      .flatMap(timestamp => buses.map(bus => (timestamp, bus, timestamp % bus)))
      .find(_._3 == 0)
      .map {case (timestamp, bus, _) => (timestamp, bus)}
  }

  def computePart1(inputs: List[String]): Long = {
    val input = parse(inputs)
    val (timestamp, bus) = computeEarliestBus(input).head
    (timestamp - input._1) * bus
  }

  def parseRow2(inputs: List[String]): Map[Long, Long] =
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

  def ei(ni: Long, n: Long): Long = {
    val niPrime = n / ni
    (0 to Int.MaxValue)
      .find(i => (i.toLong * niPrime) % ni  == 1)
      .get * niPrime
  }

  def chineseRemainderTheorem(input: Map[Long, Long]): Long = {
    val n = input.keySet.foldLeft(1L)(_ * _) // Product of all ni
    val result = input
      .map{case (ni, rest) => ei(ni, n) * rest}
      .foldLeft(0L)(_ + _)
    result % n
  }

  def compute2(input: Map[Long, Long]): Long = {
    // Adapt to Chinese Remainder Theorem
    val tmp = input.map { case (bus, i) => bus -> (-i % bus + bus) % bus }
    chineseRemainderTheorem(tmp)
  }
}
