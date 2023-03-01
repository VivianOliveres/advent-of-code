package com.kensai.aoc.aoc2020

object Day09 {

  private def parse(inputs: List[String]) =
    inputs.map(_.trim).filterNot(_.isEmpty).map(_.toLong)

  /** Return true if there is no combinations of 2 elements in `subValues` that is equal to `expectedNumber`.
    */
  private def isInvalid(expectedNumber: Long, subValues: List[Long]): Boolean =
    !subValues.combinations(2).exists(_.sum == expectedNumber)

  /** Find the first number that is not the sum of 2 of the `window` elements.
    */
  def firstInvalidNumber(window: Int, inputs: List[String]): Long =
    doFirstInvalidNumber(window, parse(inputs))

  private def doFirstInvalidNumber(window: Int, values: List[Long]): Long =
    (0 until values.size - window - 1)
      .map(i => (values(i + window), values.slice(i, i + window)))
      .filter { case (expectedNumber, subList) =>
        isInvalid(expectedNumber, subList)
      }
      .map(_._1)
      .head

  /** Find the first contiguous set where the sum is equal to `firstInvalidNumber` and return the `min+max` of this set.
    */
  def findContiguousSet(window: Int, inputs: List[String]): Long = {
    val values   = parse(inputs)
    val expected = doFirstInvalidNumber(window, values)
    val contiguousSet = (0 until values.size - window)
      .flatMap(doFindContiguousSet(_, expected, values))
      .head
    contiguousSet.min + contiguousSet.max
  }

  def doFindContiguousSet(
      currentIndex: Int,
      expectedSum: Long,
      values: List[Long]
    ): Option[List[Long]] =
    (2 to values.size)
      .map(i => values.slice(currentIndex, currentIndex + i))
      .find(_.sum == expectedSum)

}
