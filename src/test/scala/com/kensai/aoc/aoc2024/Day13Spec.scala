package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day13._
import com.kensai.aoc.lib.Lib.readRawInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day13Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readRawInputFile(
    "src/test/resources/2024/Day13.input"
  )

  private lazy val puzzleSpecInput = readRawInputFile(
    "src/test/resources/2024/Day13Spec.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result should have size 4
    result.head shouldBe Machine(Point2DL(94,34), Point2DL(22,67), Point2DL(8400,5400))
    result(1) shouldBe Machine(Point2DL(26,66), Point2DL(67,21), Point2DL(12748, 12176))
    result(2) shouldBe Machine(Point2DL(17,86), Point2DL(84, 37), Point2DL(7870, 6450))
    result(3) shouldBe Machine(Point2DL(69,23), Point2DL(27,71), Point2DL(18641, 10279))
  }

  "win(0)" should "return (80, 40)" in {
    val input = parse(puzzleSpecInput)

    When("win")
    val result = win(input.head, Some(100L))

    Then("Result is Spec Input")
    result shouldBe Some((80L, 40L))
  }

  "win(1)" should "return None" in {
    val input = parse(puzzleSpecInput)

    When("win")
    val result = win(input(1), Some(100L))

    Then("Result is Spec Input")
    result shouldBe None
  }

  "win(2)" should "return ()" in {
    val input = parse(puzzleSpecInput)

    When("win")
    val result = win(input(2), Some(100L))

    Then("Result is Spec Input")
    result shouldBe Some((38L, 86L))
  }

  "win(3)" should "return None" in {
    val input = parse(puzzleSpecInput)

    When("win")
    val result = win(input(3), Some(100L))

    Then("Result is None")
    result shouldBe None
  }

  "win(input)" should "work for Spec input" in {
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("win(input)")
    val result = win(input)

    Then("Result is 480")
    result shouldBe 480L
  }

  "win(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("win(input)")
    val result = win(input)

    Then("Result is expected")
    result shouldBe 35082L
  }

  "win and parse2 for (0)" should "return None" in {
    val input = parse2(puzzleSpecInput)

    When("win")
    val result = win(input.head, None)

    Then("Result is Spec Input")
    result shouldBe None
  }

  "win and parse2 for (1)" should "return Some" in {
    val input = parse2(puzzleSpecInput)

    When("win")
    val result = win(input(1), None)

    Then("Result is Spec Input")
    result.isDefined shouldBe true
  }

  "win and parse2 for (2)" should "return None" in {
    val input = parse2(puzzleSpecInput)

    When("win")
    val result = win(input(2), None)

    Then("Result is None")
    result shouldBe None
  }

  "win and parse2 for (3)" should "return Some" in {
    val input = parse2(puzzleSpecInput)

    When("win")
    val result = win(input(3), None)

    Then("Result is defined")
    result.isDefined shouldBe true
  }

  "win and parse2" should "work for Spec input" in {
    Given("Spec input")
    val input = parse2(puzzleSpecInput)

    When("win(input)")
    val result = win(input, None)

    Then("Result is 875318608908")
    result shouldBe 875318608908L
  }

  "win and parse2" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse2(puzzleInput)

    When("win(input)")
    val result = win(input, None)

    Then("Result is expected")
    result shouldBe 82570698600470L
  }

}
