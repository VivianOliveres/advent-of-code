package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day09._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day09Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day09.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day09Spec.input"
  )

  "findLowestPoints" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("findLowestPoints(input)")
    val result = findLowestPoints(puzzleSpecInput)

    Then("Result is [1, 0, 5, 5]")
    result shouldBe Seq(1, 0, 5, 5)
  }

  "computeRiskLevel" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeRiskLevel(input)")
    val result = computeRiskLevel(puzzleSpecInput)

    Then("Result is 15")
    result shouldBe 15
  }

  "computeRiskLevel" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeRiskLevel(puzzleInput)")
    val result = computeRiskLevel(puzzleInput)

    Then("Result is expected")
    result shouldBe 504
  }

  "findLowestAndBasins" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("findLowestAndBasins(input)")
    val result = findLowestAndBasins(puzzleSpecInput)

    Then("Result is Seq((1, 3), (0, 9), (5, 14), (5, 9))")
    result shouldBe Seq((1, 3), (0, 9), (5, 14), (5, 9))
  }

    "multiply3LargestBasinsSize" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("multiply3LargestBasinsSize(input)")
    val result = multiply3LargestBasinsSize(puzzleSpecInput)

    Then("Result is 1134")
    result shouldBe 1134
  }

  "multiply3LargestBasinsSize" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("multiply3LargestBasinsSize(puzzleInput)")
    val result = multiply3LargestBasinsSize(puzzleInput)

    Then("Result is expected")
    result shouldBe 1051087
  }
}
