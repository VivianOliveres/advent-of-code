package com.kensai.aoc.aoc2022

object Day03 {

  def findSharedItem(line: String): Char = {
    val (left, right) = line.splitAt(line.length / 2)
    val result        = left.toSet.intersect(right.toSet)
    result.head
  }

  def sumPrioritiesByElf(elves: Seq[String]): Int =
    elves
      .collect {
        case str if str.nonEmpty => str.trim
      }
      .map(findSharedItem)
      .map(toPriority)
      .sum

  private def toPriority(c: Char): Int =
    if (c.isLower)
      c.toInt - 96
    else
      c.toInt - 38

  def findSharedItem(elves: Seq[String]): Char = {
    val result = elves.foldLeft(elves.head.toSet) { case (acc, other) =>
      acc.intersect(other.toSet)
    }
    result.head
  }

  def sumPrioritiesByGroup(elves: Seq[String]): Int =
    elves
      .grouped(3)
      .map(findSharedItem)
      .map(toPriority)
      .sum
}
