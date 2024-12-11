package com.kensai.aoc.aoc2024

object Day11 {

  def parse(rows: Seq[String]): Seq[Long] =
    rows.head.split(" ").map(_.toLong).toSeq

  private def updateStone(stone: Long): Seq[Long] =
    if (stone == 0L)
      Seq(1L)
    else {
      val headStr = stone.toString
      if (headStr.length % 2 == 0) {
        val (left, right) = headStr.splitAt(headStr.length / 2)
        Seq(left.toLong, right.toLong)
      } else
        Seq(stone * 2024L)
    }

  private def doBlink(counts: Map[Long, Long]): Map[Long, Long] =
    counts.toSeq
      .flatMap { case (stone, count) => updateStone(stone).map(s => (s, count)) }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sum)
      .toMap

  def blink(input: Seq[Long], times: Int): Long = {
    val counts = input.groupBy(identity).view.mapValues(_.size.toLong).toMap
    (0 until times)
      .foldLeft(counts) { case (previousState, _) =>
        doBlink(previousState)
      }
      .values
      .sum
  }

}
