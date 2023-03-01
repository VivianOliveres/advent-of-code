package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day23._
import com.kensai.aoc.aoc2021.Day23.Amphipod._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day23Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day23.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day23Spec.input"
  )

  "parsePart1" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parsePart1(puzzleSpecInput)")
    val result = parsePart1(puzzleSpecInput)

    Then("Result is expected")
    result.totalEnergySpent shouldBe 0
    result.roomSize shouldBe 2
    result.positions.get(Point2D(2, 1)) should contain(Bronze)
    result.positions.get(Point2D(2, 2)) should contain(Amber)
    result.positions.get(Point2D(4, 1)) should contain(Copper)
    result.positions.get(Point2D(4, 2)) should contain(Desert)
    result.positions.get(Point2D(6, 1)) should contain(Bronze)
    result.positions.get(Point2D(6, 2)) should contain(Copper)
    result.positions.get(Point2D(8, 1)) should contain(Desert)
    result.positions.get(Point2D(8, 2)) should contain(Amber)
  }

  "computeBestSolution" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parsePart1(puzzleSpecInput)

    When("computeBestSolution(puzzleSpecInput)")
    val result = computeBestSolution(input)

    Then("Result is 12521")
    result shouldBe 12521
  }

  "computeBestSolution" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parsePart1(puzzleInput)

    When("computeBestSolution(puzzleSpecInput)")
    val result = computeBestSolution(input)

    Then("Result is expected")
    result shouldBe 16508
  }

  "parsePart2" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parsePart2(puzzleSpecInput)")
    val result = parsePart2(puzzleSpecInput)

    Then("Result is expected")
    result.totalEnergySpent shouldBe 0
    result.roomSize shouldBe 4
    result.positions.get(Point2D(2, 1)) should contain(Bronze)
    result.positions.get(Point2D(2, 2)) should contain(Desert)
    result.positions.get(Point2D(2, 3)) should contain(Desert)
    result.positions.get(Point2D(2, 4)) should contain(Amber)
    result.positions.get(Point2D(4, 1)) should contain(Copper)
    result.positions.get(Point2D(4, 2)) should contain(Copper)
    result.positions.get(Point2D(4, 3)) should contain(Bronze)
    result.positions.get(Point2D(4, 4)) should contain(Desert)
    result.positions.get(Point2D(6, 1)) should contain(Bronze)
    result.positions.get(Point2D(6, 2)) should contain(Bronze)
    result.positions.get(Point2D(6, 3)) should contain(Amber)
    result.positions.get(Point2D(6, 4)) should contain(Copper)
    result.positions.get(Point2D(8, 1)) should contain(Desert)
    result.positions.get(Point2D(8, 2)) should contain(Amber)
    result.positions.get(Point2D(8, 3)) should contain(Copper)
    result.positions.get(Point2D(8, 4)) should contain(Amber)
  }

  "computeBestSolution" should "find result from spec input for part2" in {
    Given("Puzzle spec input")
    val input = parsePart2(puzzleSpecInput)

    When("computeBestSolution(puzzleSpecInput)")
    val result = computeBestSolution(input)

    Then("Result is 44169")
    result shouldBe 44169
  }

  "computeBestSolution" should "find result from input for part2" in {
    Given("Puzzle spec input")
    val input = parsePart2(puzzleInput)

    When("computeBestSolution(puzzleSpecInput)")
    val result = computeBestSolution(input)

    Then("Result is expected")
    result shouldBe 43626
  }
}
