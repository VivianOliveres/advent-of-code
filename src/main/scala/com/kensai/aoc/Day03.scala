package com.kensai.aoc

object Day03 {

  /**
   * Raw representation with row index {@code x} , {@code rowSize} and the position of all trees {@code trees}.
   */
  case class TobogganRow(x: Int, rowSize: Int, trees: Set[Int])

  def parse(input: String, x: Int): TobogganRow = {
    try {
      val result = input.zipWithIndex
        .filter(_._1 == '#')
        .map(_._2)
        .toSet
      TobogganRow(x, input.length, result)

    } catch {
      case _: RuntimeException => {
        System.err.println(s"Day03 - parse - Invalid format for input[$input]")
        TobogganRow(x, input.length, Set())
      }
    }
  }

  /**
   * Count the number of trees found when starting from (0, 0) position and incrementing current position by ({@code xSlope}, {@code ySlope}).
   */
  def countTrees(xSlope: Int, ySlope: Int, input: Map[Int, TobogganRow]): Long = {
    var x = 0
    var y = 0
    var counter = 0
    while(y < input.size) {
      val row = input(y)
      // Increment counter
      if (row.trees.contains(x % row.rowSize))
        counter = counter + 1

      // Increment position
      x = x + xSlope
      y = y + ySlope
    }
    counter
  }

  /**
   * For multiple {@code slopes}, count all trees and return the multiplication.
   */
  def countMultipleTrees(slopes: List[(Int, Int)], input: Map[Int, TobogganRow]): Long =
    slopes.map(slope => countTrees(slope._1, slope._2, input))
      .foldLeft(1L)(_ * _)
}
