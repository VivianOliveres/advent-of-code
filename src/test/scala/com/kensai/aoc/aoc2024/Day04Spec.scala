package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day04._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day04Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day04.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day04Spec.input"
  )

  "countAll(countXmasAt)" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("countAll(input, countXmasAt)")
    val result = countAll(input, countXmasAt)

    Then("Result is 18")
    result shouldBe 18
  }

  "countAll(countXmasAt)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = puzzleInput

    When("countAll(input, countXmasAt)")
    val result = countAll(input, countXmasAt)

    Then("Result is expected")
    result shouldBe 2414
  }

  "countAll(countMasAt)" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("countAll(input)")
    val result = countAll(input, countMasAt)

    Then("Result is 9")
    result shouldBe 9
  }

  "countAll(countMasAt)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = puzzleInput

    When("countAll(input")
    val result = countAll(input, countMasAt)

    Then("Result is expected")
    result shouldBe 1871
  }

}
