package com.kensai.aoc.aoc2024

object Day07 {

  type Operation = (Long, Long) => Long
  val Part1Operations: Seq[Operation] =
    Seq(_ + _, _ * _)
  val Part2Operations: Seq[Operation] =
    Seq(_ + _, _ * _, (left, right) => (left.toString + right.toString).toLong)

  def parse(rows: Seq[String]): Seq[(Long, Seq[Long])] =
    rows.filterNot(_.isEmpty).map { row =>
      val splited = row.split(": ")
      val result  = splited.head.trim.toLong
      val numbers = splited(1).split(" ").toSeq.map(s => s.trim.toLong)
      (result, numbers)
    }

  def countValidEquations(input: Seq[(Long, Seq[Long])], operations: Seq[Operation]): Long =
    input
      .filter { case (expected, numbers) => checkEquation(expected, numbers, operations) }
      .map { case (expected, _) => expected }
      .sum

  def checkEquation(expected: Long, numbers: Seq[Long], operations: Seq[Operation]): Boolean =
    operations.exists(op => doCheckEquation(expected, op(numbers.head, numbers(1)), numbers.tail.tail, operations))

  private def doCheckEquation(expected: Long, currentResult: Long, remainingNumbers: Seq[Long], operations: Seq[Operation])
      : Boolean =
    if (currentResult == expected && remainingNumbers.isEmpty)
      true
    else if (currentResult > expected)
      false
    else if (remainingNumbers.isEmpty)
      false
    else
      operations
        .exists(op => doCheckEquation(expected, op(currentResult, remainingNumbers.head), remainingNumbers.tail, operations))

}
