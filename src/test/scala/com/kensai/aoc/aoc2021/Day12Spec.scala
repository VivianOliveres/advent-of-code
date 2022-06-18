package com.kensai.aoc.aoc2021

import Day12._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day12Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputLines(
    "src/test/resources/2021//Day12.input"
  )
  private lazy val specInputs = readInputLines(
    "src/test/resources/2021//Day12Spec.input"
  )

  "parse commands from specs" should "return 5 commands" in {
    val input =
      """
        |F10
        |N3
        |F7
        |R90
        |F11
        |""".stripMargin
    Given(s"Input is \n$input")

    When("parse")
    val result = parse(input.split("\n").toList)

    val expectedResult =
      List(Forward(10), North(3), Forward(7), Right(90), Forward(11))
    Then(s"Result is $expectedResult")
    result shouldBe expectedResult
  }

  "computeManhattanDistance from Pos(0, 0, E) based on specs" should "return 25" in {
    Given(s"Input is spec")

    When("computeManhattanDistance")
    val result = computeManhattanDistance(Pos(0, 0, E), specInputs)

    val expectedResult = 25
    Then(s"Result is $expectedResult")
    result shouldBe expectedResult
  }

  "computeManhattanDistance from Pos(0, 0, E) based on puzzle inputs" should "return solution" in {
    Given(s"Puzzle input")

    When("computeManhattanDistance")
    val result = computeManhattanDistance(Pos(0, 0, E), puzzleInputs)

    val expectedResult = 1687
    Then(s"Result is $expectedResult")
    result shouldBe 1687
  }

  "computeManhattanDistance2(Pos(0, 0, E), Pos(10, 1, N)) from spec" should "return 286" in {
    Given(s"Input is spec")

    When("computeManhattanDistance2(Pos(0, 0, E), Pos(10, 1, N), specInputs)")
    val result =
      computeManhattanDistance2(Pos(0, 0, E), Pos(10, 1, N), specInputs)

    val expectedResult = 286
    Then(s"Result is $expectedResult")
    result shouldBe expectedResult
  }

  "computeManhattanDistance2 from puzzle" should "return solution" in {
    Given(s"Input is spec")

    When("computeManhattanDistance2(Pos(0, 0, E), Pos(10, 1, N), puzzleInputs)")
    val result =
      computeManhattanDistance2(Pos(0, 0, E), Pos(10, 1, N), puzzleInputs)

    val expectedResult = 20873
    Then(s"Result is $expectedResult")
    result shouldBe expectedResult
  }

}
