package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day01._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day01Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day01.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day01Spec.input"
  )

  "compute(1)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("sumMaxCalories(input, 1)")
    val result = sumMaxCalories(puzzleSpecInput, 1)

    Then("Result is 24000")
    result shouldBe 24000
  }

  "compute(1)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("sumMaxCalories(input, 1)")
    val result = sumMaxCalories(puzzleInput, 1)

    Then("Result is expected")
    result shouldBe 69501
  }

  "compute(3)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("sumMaxCalories(input, 3)")
    val result = sumMaxCalories(puzzleSpecInput, 3)

    Then("Result is 45000")
    result shouldBe 45000
  }

  "compute(3)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("sumMaxCalories(input, 3)")
    val result = sumMaxCalories(puzzleInput, 3)

    Then("Result is expected")
    result shouldBe 202346
  }
}
