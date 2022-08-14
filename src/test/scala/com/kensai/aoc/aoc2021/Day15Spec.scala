package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day15._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day15.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day15Spec.input"
  )

  private lazy val puzzleSpecGeneratedInput = readInputLines(
    "src/test/resources/2021/Day15SpecGenerated.input"
  )

  "parse inputs" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val result = parse(puzzleSpecInput)

    Then("Result is parsed")
    result.positions should have size 100
  }

  "computeLowestRiskPath" should "find result from spec input" in {
    Given("Puzzle input")

    When("computeLowestRiskPath(puzzleInput)")
    val inputs = parse(puzzleSpecInput)
    val result = computeLowestRiskPath(inputs)

    Then("Result is 40")
    result shouldBe 40
  }

  "computeLowestRiskPath" should "find result from input" in {
    Given("Puzzle input")

    When("computeLowestRiskPath(puzzleInput)")
    val inputs = parse(puzzleInput)
    val result = computeLowestRiskPath(inputs)

    Then("Result is expected")
    result shouldBe 373
  }

  "parseHigherMap inputs" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parseHigherMap(puzzleSpecInput)")
    val result = parseBiggerMap(puzzleSpecInput)

    Then("Result is parsed")
    result.positions should have size 50 * 50

    val expected = parse(puzzleSpecGeneratedInput)
    result.positions shouldBe expected.positions
  }

  "computeLowestRiskPath with 5 times higher map" should "find result from spec input" in {
    Given("Puzzle input")

    When("computeLowestRiskPath(puzzleInput)")
    val inputs = parseBiggerMap(puzzleSpecInput)
    val result = computeLowestRiskPath(inputs)

    Then("Result is 315")
    result shouldBe 315
  }

  // TODO: improve performance (with a mutable priority queue ?) because it takes 9 seconds to run
  "computeLowestRiskPath with 5 times higher map" should "find result from input" in {
    Given("Puzzle input")

    When("computeLowestRiskPath(puzzleInput)")
    val inputs = parseBiggerMap(puzzleInput)
    val result = computeLowestRiskPath(inputs)

    Then("Result is expected")
    result shouldBe 2868
  }
}
