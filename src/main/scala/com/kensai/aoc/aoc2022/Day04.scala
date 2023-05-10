package com.kensai.aoc.aoc2022

object Day04 {

  case class RangeDay4(leftMin: Int, leftMax: Int, rightMin: Int, rightMax: Int) {
    def isSelfIncluded: Boolean =
      (leftMin <= rightMin && leftMax >= rightMax) ||
      (rightMin <= leftMin && rightMax >= leftMax)

    def isOverlapped: Boolean =
      leftMax >= rightMin && leftMin <= rightMax
  }

  private val rowRegex = """(\d+)-(\d+),(\d+)-(\d+)""".r
  def parse(lines: Seq[String]): Seq[RangeDay4] =
    lines.collect {
      case str if str.nonEmpty => str.trim
    }.map {
      case rowRegex(leftMinStr, leftMaxStr, rightMinStr, rightMaxStr) =>
        RangeDay4(leftMinStr.toInt, leftMaxStr.toInt, rightMinStr.toInt, rightMaxStr.toInt)
      case str => throw new IllegalArgumentException(s"Invalid line [$str]")
    }

  def countSelfIncluded(groups: Seq[RangeDay4]): Int =
    groups.count(_.isSelfIncluded)

  def countOverlapped(groups: Seq[RangeDay4]): Int =
    groups.count(_.isOverlapped)


}
