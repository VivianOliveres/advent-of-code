package com.kensai.aoc

import scala.collection.mutable

object Day15 {

  def parse(inputs: String): List[Long] =
    inputs
      .split("\n")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .head
      .split(",")
      .map(_.toLong)
      .toList

  def lastSpokenAt(firstNumbers: List[Long], expectedTurn: Long): Long = {
    var currentState: mutable.Map[Long, Long] =
      firstNumbers
        .zipWithIndex
        .map { case (num, index) => num -> (index + 1L) }
        .to(mutable.Map) // Mutable map increase perf by 66% in this case
    var lastSpoken = firstNumbers.last
    var turn = firstNumbers.size + 1L
    while (turn < expectedTurn + 1L) {
      if (currentState.contains(lastSpoken) && currentState(lastSpoken) != turn - 1) {
        val previousTurn = currentState(lastSpoken)
        currentState += (lastSpoken -> (turn - 1))
        val newLastSpoken = turn - 1 - previousTurn
        lastSpoken = newLastSpoken
      } else {
        currentState += (lastSpoken -> (turn - 1))
        lastSpoken = 0L
      }
      turn = turn + 1
    }
    lastSpoken
  }

}
