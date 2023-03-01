package com.kensai.aoc.aoc2020

import scala.annotation.tailrec

object Day10 {

  private def parse(inputs: List[String]): Set[Long] =
    inputs
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toLong)
      .toSet

  @tailrec
  def doComputeDiff(
      currentValue: Long,
      acc: (Long, Long, Long),
      values: Set[Long]
    ): (Long, Long, Long) =
    if (values.contains(currentValue + 1))
      doComputeDiff(currentValue + 1, (acc._1 + 1, acc._2, acc._3), values)
    else if (values.contains(currentValue + 2))
      doComputeDiff(currentValue + 2, (acc._1, acc._2 + 1, acc._3), values)
    else if (values.contains(currentValue + 3))
      doComputeDiff(currentValue + 3, (acc._1, acc._2, acc._3 + 1), values)
    else (acc._1, acc._2, acc._3 + 1)

  def computeDiff(inputs: List[String]): (Long, Long, Long) = {
    val values = parse(inputs)
    doComputeDiff(0L, (0L, 0L, 0L), values)
  }

  def computeResult(inputs: List[String]): Long = {
    val values            = parse(inputs)
    val (diff1, _, diff3) = doComputeDiff(0L, (0L, 0L, 0L), values)
    diff1 * diff3
  }

  /** Compute the maximum (+3) value that can be reached by moving from 1 to 3.
    */
  @tailrec
  private def doComputeMax(currentValue: Long, values: Set[Long]): Long =
    if (values.contains(currentValue + 1))
      doComputeMax(currentValue + 1, values)
    else if (values.contains(currentValue + 2))
      doComputeMax(currentValue + 2, values)
    else if (values.contains(currentValue + 3))
      doComputeMax(currentValue + 3, values)
    else currentValue + 3

  /** Compute the number of paths that reached the max according to movement rules.
    */
  def pathReachingMaxCount(inputs: List[String]): Long = {
    val values = parse(inputs)
    val max    = doComputeMax(0L, values)
    val acc = (0L to max)
      .filter(values.contains)
      .map(_ -> 0L)
      .toMap
      .updated(0L, 1L)  // First step is 1
      .updated(max, 0L) // Add last step
    doCompute2(0L, max, acc, values)
  }

  /** Compute recursively the number of paths that reached `max` by updating the accumulator.
    *
    * @param currentKey
    *   : Value in this recursive step (from `values`).
    * @param max
    *   : Value that ends recursion.
    * @param acc
    *   : Keys (from input file) to number of paths that reached this key.
    * @param values
    *   : All values from input file.
    * @return
    *   : Number of paths that reached `max` step (from `values`).
    */
  @tailrec
  private def doCompute2(
      currentKey: Long,
      max: Long,
      acc: Map[Long, Long],
      values: Set[Long]
    ): Long =
    if (currentKey >= max)
      acc(max)
    else {
      if (acc.contains(currentKey)) {
        val newState = (2 to 3).foldLeft(update(currentKey, acc, 1))(
          update(currentKey, _, _)
        )
        doCompute2(currentKey + 1, max, newState, values)
      } else {
        doCompute2(currentKey + 1, max, acc, values)
      }
    }

  private def update(
      key: Long,
      acc: Map[Long, Long],
      keyInc: Int
    ): Map[Long, Long] =
    if (acc.contains(key + keyInc)) {
      val value = acc(key)
      val t     = acc(key + keyInc)
      acc.updated(key + keyInc, value + t)
    } else
      acc

}
