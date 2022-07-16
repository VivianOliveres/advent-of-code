package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day08._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day08Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day08.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day08Spec.input"
  )

  "countDigits147" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countDigits147(input)")
    val result = countDigits147(puzzleSpecInput)

    Then("Result is 26")
    result shouldBe 26
  }

  "countDigits147" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("countDigits147(puzzleInput)")
    val result = countDigits147(puzzleInput)

    Then("Result is expected")
    result shouldBe 530
  }


  "computeDigitValue" should "find result from one input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parseInput(Seq("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")).head

    When("computeDigitValue(input)")
    val result = computeDigitValue(input)

    Then("Result is 5353")
    result shouldBe 5353
  }

    "computeDigitValue" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeDigitValue(input)")
    val result = computeDigitValue(puzzleSpecInput)

    Then("Result is 61229")
    result shouldBe 61229
  }

  "computeDigitValue" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeDigitValue(puzzleInput)")
    val result = computeDigitValue(puzzleInput)

    Then("Result is expected")
    result shouldBe 1051087
  }
}
