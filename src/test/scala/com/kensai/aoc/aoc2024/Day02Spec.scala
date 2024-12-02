package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day02._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day02Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day02.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day02Spec.input"
  )

  "parse(Spec)" should "return Reports" in {
    Given("Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is 6 reports of 5 levels")
    result should have size 6
    result.foreach { report =>
      report.levels should have size 5
    }
  }

  "countSafe" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("countSafe(input)")
    val result = countSafe(input)

    Then("Result is 2")
    result shouldBe 2
  }

  "countSafe" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countSafe(input")
    val result = countSafe(input)

    Then("Result is expected")
    result shouldBe 390
  }

  "countDampenerSafe" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("countDampenerSafe(input)")
    val result = countDampenerSafe(input)

    Then("Result is 4")
    result shouldBe 4
  }

  "countDampenerSafe" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countDampenerSafe(input)")
    val result = countDampenerSafe(input)

    Then("Result is expected")
    result shouldBe 439
  }

}
