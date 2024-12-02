package com.kensai.aoc.aoc2024

import scala.annotation.tailrec

object Day02 {

  case class Report(levels: Seq[Int])

  def parse(row: String): Report =
    Report(row.split(" ").toSeq.map(_.toInt))

  def parse(rows: Seq[String]): Seq[Report] =
    rows
      .filterNot(_.isEmpty)
      .map(parse)

  def countSafe(reports: Seq[Report]): Int =
    reports
      .count(isSafe)

  private def isSafe(report: Report): Boolean =
    doIsSafe(report, 0, 1, report.levels.head > report.levels(1))

  @tailrec
  private def doIsSafe(report: Report, currentIndex: Int, nextIndex: Int, isDecreasing: Boolean): Boolean =
    if (nextIndex >= report.levels.size)
      true
    else {
      val currentLevel = report.levels(currentIndex)
      val nextLevel    = report.levels(nextIndex)
      if (isSafe(currentLevel, nextLevel, isDecreasing))
        doIsSafe(report, currentIndex + 1, nextIndex + 1, isDecreasing)
      else
        false
    }

  private def isSafe(currentLevel: Int, nextLevel: Int, isDecreasing: Boolean): Boolean =
    if (math.abs(currentLevel - nextLevel) > 3 || currentLevel == nextLevel)
      false
    else if (isDecreasing && currentLevel < nextLevel)
      false
    else if (!isDecreasing && currentLevel > nextLevel)
      false
    else
      true

  def countDampenerSafe(reports: Seq[Report]): Int =
    reports
      .count(isDampenerSafe)

  private def getNextIndex(index: Int, skipIndex: Int): Int =
    if (index == skipIndex)
      index + 1
    else index

  /**
   * Brut force: Find first solution that works with one skipped index (-1 index is when everything works).
   */
  private def isDampenerSafe(report: Report): Boolean =
    (-1 to report.levels.size).exists { skippedIndex =>
      val currentIndex = getNextIndex(0, skippedIndex)
      val nextIndex = getNextIndex(currentIndex + 1, skippedIndex)
      val isDecreasing = report.levels(currentIndex) > report.levels(nextIndex)
      doIsDampenerSafe(report, currentIndex, nextIndex, isDecreasing, skippedIndex)
    }

  @tailrec
  private def doIsDampenerSafe(report: Day02.Report, currentIndex: Int, nextIndex: Int, isDecreasing: Boolean, skippedIndex: Int): Boolean =
    if (nextIndex >= report.levels.size)
      true // Out of range
    else {
      val currentLevel = report.levels(currentIndex)
      val nextLevel    = report.levels(nextIndex)
      if (isSafe(currentLevel, nextLevel, isDecreasing)) {
        // Ii is Safe => Next step without skipped index
        val nextCurrentIndex = nextIndex
        val nextNextIndex    = getNextIndex(nextCurrentIndex + 1, skippedIndex)
        doIsDampenerSafe(report, nextCurrentIndex, nextNextIndex, isDecreasing, skippedIndex)
      } else {
        // Not safe
        false
      }
    }

}
