package com.kensai.aoc.aoc2024

object Day01 {

  def parse(rows: Seq[String]): (Seq[Long], Seq[Long]) =
    rows
      .filterNot(_.isEmpty)
      .map(doParse)
      .foldLeft((Seq.empty[Long], Seq.empty[Long])){ case ((accLeft, accRight), (left, right)) =>
        (accLeft :+ left, accRight :+ right)
      }

  private def doParse(row: String): (Long, Long) = {
    val cleaned = row
      .split(" ")
      .toList
      .filterNot(_.isEmpty)
    (cleaned.head.toLong, cleaned(1).toLong)
  }

  def shortestTotalDistance(input: (Seq[Long], Seq[Long])): Long = {
    val left = input._1.sorted
    val right = input._2.sorted
    left
      .zip(right)
      .map { case (l, r) => math.abs(l - r) }
      .sum
  }

  def similarityScore(input: (Seq[Long], Seq[Long])): Long = {
    val rightCount = input._2
      .groupBy(identity)
      .view.mapValues(_.size.toLong)
      .toMap
    input._1
      .map(l => l * rightCount.getOrElse(l, 0L))
      .sum
  }
}
