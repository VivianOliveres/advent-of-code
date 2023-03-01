package com.kensai.aoc.aoc2021

object Day07 {

  private def parseInput(input: String): Seq[Int] =
    input.trim.split(",").map(_.trim.toInt).toSeq

  /** Compute the min fuel needed to align all crabs. Every movement cost 1 fuel.
    */
  def computeMinFuel(input: String): Int = {
    val inputs = parseInput(input)
    computer(inputs, identity)
  }

  private def computer(inputs: Seq[Int], modifier: Int => Int): Int =
    (0 to inputs.max)
      .map(index => inputs.map(crabPos => math.abs(index - crabPos)).map(modifier).sum)
      .min

  /** Compute the min fuel needed to align all crabs. First movement cost 1 fuel, second 2 fuels, third 3 fuels, ... At the end it costs the
    * sum of movements (ie n*(n+1)/2).
    */
  def computeMinFuel2(input: String): Int = {
    val inputs = parseInput(input)
    computer(inputs, value => value * (value + 1) / 2)
  }

}
