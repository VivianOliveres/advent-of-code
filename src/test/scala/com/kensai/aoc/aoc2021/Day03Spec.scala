package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day03._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day03Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day03.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day03Spec.input"
  )

  "computePowerConsumption" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computePowerConsumption(input)")
    val result = computePowerConsumption(puzzleSpecInput)

    Then("Result is 198")
    result shouldBe 198
  }

  "computePowerConsumption" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computePowerConsumption(puzzleInput)")
    val result = computePowerConsumption(puzzleInput)

    Then("Result is expected")
    result shouldBe 3901196
  }

  "computeLifeSupportRating" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeLifeSupportRating(input)")
    val result = computeLifeSupportRating(puzzleSpecInput)

    Then("Result is 230")
    result shouldBe 23 * 10
  }

  "computeLifeSupportRating" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeLifeSupportRating(puzzleInput)")
    val result = computeLifeSupportRating(puzzleInput)

    Then("Result is expected")
    result shouldBe 4412188
  }
}
