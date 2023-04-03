package com.kensai.aoc.aoc2021

import scala.collection.mutable

object Day24 {
  case class Step(a: Int, b: Int, c: Int)
  object Step {
    def apply(lines: Seq[String]): Step = (lines(4), lines(5), lines(15)) match {
      case (s"div z $a", s"add x $b", s"add y $c") => Step(a.toInt, b.toInt, c.toInt)
      case _                                       => throw new IllegalArgumentException(s"Invalid input:\n$lines")
    }
  }

  def findLargestModelNumber(steps: Seq[Step]): Long = {
    val result = doCompute(steps, false, 9)
    println(s"Find solution [$result]")
    result
  }

  private def doCompute(steps: Seq[Step], part2: Boolean, best: Int): Long = {
    val result = mutable.IndexedSeq.fill(14)(-1)
    var buffer = Seq[(Int, Int)]()
    steps.zipWithIndex.foreach { case (step, index) =>
      if (step.a == 26) {
        val offset                  = step.b
        val (lastIndex, lastOffset) = buffer.head
        buffer = buffer.tail
        val difference = offset + lastOffset
        if (difference >= 0) {
          result(lastIndex) = if (part2) best else best - difference
          result(index) = if (part2) best + difference else best
        } else {
          result(lastIndex) = if (part2) best - difference else best
          result(index) = if (part2) best else best + difference
        }

      } else {
        buffer = (index, step.c) +: buffer
      }
    }
    result.mkString.toLong
  }

  def findSmallestModelNumber(steps: Seq[Step]): Long = {
    val result = doCompute(steps, true, 1)
    println(s"Find solution [$result]")
    result
  }

  def parse(lines: Seq[String]): Seq[Step] =
    doParse(lines.map(_.trim).filterNot(_.isEmpty))

  def doParse(lines: Seq[String]): Seq[Step] = lines.grouped(18).map(Step(_)).toSeq
}
