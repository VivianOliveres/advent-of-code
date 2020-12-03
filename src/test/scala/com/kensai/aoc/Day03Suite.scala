package com.kensai.aoc

import com.kensai.aoc.Day03._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day03Suite extends AnyFlatSpec {

  private val SpecInputPath = "src/test/resources/Day03Spec.input"
  private val InputPath = "src/test/resources/Day03.input"

  private val Row1: String = "..##......."
  private val ExpectedRow1: TobogganRow = TobogganRow(0, 11, Set(2, 3))

  private val Row2: String = "#...#...#.."
  private val ExpectedRow2: TobogganRow = TobogganRow(1, 11, Set(0, 4, 8))

  "parse Row1" should "return a valid ExpectedRow1" in {
    // GIVEN: input from specs
    val input = Row1

    // WHEN: parse
    val result = parse(input, 0)

    // THEN: result is parsed
    result should be (ExpectedRow1)
  }

  "parse Row2" should "return a valid ExpectedRow2" in {
    // GIVEN: input from specs
    val input = Row2

    // WHEN: parse
    val result = parse(input, 1)

    // THEN: result is parsed
    result should be (ExpectedRow2)
  }

  "countTrees(3, 1) for specs" should "return 7" in {
    // GIVEN: input from specs
    val input = readInputFile(SpecInputPath)

    // WHEN: countTrees(3, 1)
    val result = countTrees(3, 1, input)

    // THEN: result is parsed
    result should be (7)
  }

  "countTrees(3, 1)" should "return solution" in {
    // GIVEN: input from specs
    val input = readInputFile(InputPath)

    // WHEN: countTrees(3, 1)
    val result = countTrees(3, 1, input)

    // THEN: result is parsed
    println(result)
    result should be (299)
  }

  "countMultipleTrees for spec" should "return solution" in {
    // GIVEN: input from specs
    val input = readInputFile(SpecInputPath)

    // WHEN: countMultipleTrees
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    val result = countMultipleTrees(slopes, input)

    // THEN: result is 336L
    result should be (336L)
  }

  "countMultipleTrees" should "return solution" in {
    // GIVEN: input
    val input = readInputFile(InputPath)

    // WHEN: countMultipleTrees
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    val result = countMultipleTrees(slopes, input)

    // THEN: result is parsed
    println(result)
    result should be (3621285278L)
  }

  private def readInputFile(path: String): Map[Int, TobogganRow] =
    Source.fromFile(path)
      .getLines
      .toList
      .filterNot(_.isEmpty)
      .zipWithIndex
      .map {case (row, y) => (row.trim, y)}
      .map{case (row, y) => parse(row, y)}
      .map(row => row.x -> row)
      .toMap


}
