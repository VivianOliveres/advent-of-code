package com.kensai.aoc.aoc2024

import scala.collection.mutable


object Day19 {

  case class Day19Input(available: Set[String], desired: Seq[String])

  def parse(rows: Seq[String]): Day19Input = {
    val available = rows.head.split(", ").toSet
    val desired = rows.tail.tail
    Day19Input(available, desired)
  }

  def isPossible(available: Set[String], desired: String): Boolean = {
    val newAvailable = available.filter(a => desired.contains(a)) // Opti filter available
    val memo: mutable.Map[String, Boolean] = mutable.Map()
    doIsPossible(available = newAvailable, desired = desired, memo = memo)
  }

  private def doIsPossible(available: Set[String], desired: String, memo: mutable.Map[String, Boolean]): Boolean = {
    if (desired.isEmpty)
      true
    else if (memo.contains(desired))
      memo(desired)
    else {
      val result = available
        .filter(a => desired.startsWith(a))
        .exists{a => {
          val newDesired = desired.substring(a.length)
          doIsPossible(available, newDesired, memo)
        }}
      memo.update(desired, result)
      result
    }
  }

  def countPossible(input: Day19Input): Int =
    input.desired.count(p => isPossible(input.available, p))

  //TODO: why is it possible
  def sumPossible(patterns: Seq[String], design: String): Long = {
    val memo = mutable.Map.empty[Int, Long]

    def helper(i: Int): Long = {
      memo.getOrElseUpdate(i, {
        if (i == design.length)
          1
        else {
          (for {
            pattern <- patterns
            if design.startsWith(pattern, i)
          } yield helper(i + pattern.length)).sum
        }
      })
    }

    helper(0)
  }

  def sumPossible(input: Day19Input): Long =
    input.desired.map(sumPossible(input.available.toSeq, _)).sum
}
