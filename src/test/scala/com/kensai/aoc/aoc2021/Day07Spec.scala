package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day07._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day07Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day07.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day07Spec.input"
  )

  "computeMinFuel" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeMinFuel(input)")
    val result = computeMinFuel(puzzleSpecInput.head)

    Then("Result is 37")
    result shouldBe 37
  }

  "computeMinFuel" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeMinFuel(puzzleInput)")
    val result = computeMinFuel(puzzleInput.head)

    Then("Result is expected")
    result shouldBe 356922
  }

  "computeMinFuel2" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeMinFuel2(input)")
    val result = computeMinFuel2(puzzleSpecInput.head)

    Then("Result is 168")
    result shouldBe 168
  }

  "computeMinFuel2" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeMinFuel2(puzzleInput)")
    val result = computeMinFuel2(puzzleInput.head)

    Then("Result is expected")
    result shouldBe 100347031
  }
}
