package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day07._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day07Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day07.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day07Spec.input"
  )

  "parse" should "work for Spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result should have size 9
    result.head shouldBe (190, Seq(10, 19))
    result(1) shouldBe (3267, Seq(81, 40, 27))
  }

  "checkEquation(190, Seq(10, 19))" should "be true" in {
    When("checkEquation(190, Seq(10, 19))")
    val result = checkEquation(190, Seq(10, 19), Part1Operations)

    Then("Result is true")
    result shouldBe true
  }

  "checkEquation(3267, Seq(81, 40, 27))" should "be true" in {
    When("checkEquation(3267, Seq(81, 40, 27))")
    val result = checkEquation(3267, Seq(81, 40, 27), Part1Operations)

    Then("Result is true")
    result shouldBe true
  }

  "checkEquation(292, Seq(11, 6, 16, 20))" should "be true" in {
    When("checkEquation(292, Seq(11, 6, 16, 20)))")
    val result = checkEquation(292, Seq(11, 6, 16, 20), Part1Operations)

    Then("Result is true")
    result shouldBe true
  }

  "checkEquation(21037, Seq(9, 7, 18, 13))" should "be false" in {
    When("checkEquation(292, Seq(11, 6, 16, 20)))")
    val result = checkEquation(21037, Seq(9, 7, 18, 13), Part1Operations)

    Then("Result is false")
    result shouldBe false
  }

  "countValidEquations(input, Part1Operations)" should "find result from spec" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("countValidEquations(input, Part1Operations)")
    val result = countValidEquations(input, Part1Operations)

    Then("Result is 3749")
    result shouldBe 3749L
  }

  "countValidEquations(input, Part1Operations)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countValidEquations(input, Part1Operations)")
    val result = countValidEquations(input, Part1Operations)

    Then("Result is expected")
    result shouldBe 10741443549536L
  }

  "checkEquation2(156, Seq(15, 6))" should "be true" in {
    When("checkEquation2(156, Seq(15, 6))")
    val result = checkEquation(156, Seq(15, 6), Part2Operations)

    Then("Result is true")
    result shouldBe true
  }

  "checkEquation2(7290, Seq(6, 8, 6, 15))" should "be true" in {
    When("checkEquation2(7290, Seq(6, 8, 6, 15))")
    val result = checkEquation(7290, Seq(6, 8, 6, 15), Part2Operations)

    Then("Result is true")
    result shouldBe true
  }

  "countValidEquations(input, Part2Operations)" should "find result from spec" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("countValidEquations(input, Part2Operations)")
    val result = countValidEquations(input, Part2Operations)

    Then("Result is 11387")
    result shouldBe 11387L
  }

  "countValidEquations(input, Part2Operations)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countValidEquations(input, Part2Operations)")
    val result = countValidEquations(input, Part2Operations)

    Then("Result is expected")
    result shouldBe 500335179214836L
  }

}
