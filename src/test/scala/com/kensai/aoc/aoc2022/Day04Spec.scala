package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day04._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day04Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day04.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day04Spec.input"
  )

  "parse" should "return RangeDay4 from spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.head shouldBe RangeDay4(2, 4, 6, 8)
    result(1) shouldBe RangeDay4(2, 3, 4, 5)
    result(2) shouldBe RangeDay4(5, 7, 7, 9)
    result(3) shouldBe RangeDay4(2, 8, 3, 7)
    result(4) shouldBe RangeDay4(6, 6, 4, 6)
    result(5) shouldBe RangeDay4(2, 6, 4, 8)
    result should have size 6
  }

  "isSelfIncluded" should "return false for [2-4,6-8]" in {
    // GIVEN: input
    Given("[2-4,6-8] input")
    val range = RangeDay4(2, 4, 6, 8)

    When("isSelfIncluded")
    val result = range.isSelfIncluded

    Then("Result is false")
    result shouldBe false
  }

  "isSelfIncluded" should "return true for [6-6,4-6]" in {
    // GIVEN: input
    Given("[6-6,4-6] input")
    val range = RangeDay4(6, 6, 4, 6)

    When("isSelfIncluded")
    val result = range.isSelfIncluded

    Then("Result is true")
    result shouldBe true
  }

  "countSelfIncluded" should "return result from Spec Input" in {
    // GIVEN: input
    Given("Input")
    val input = parse(puzzleSpecInput)

    When("countSelfIncluded(input)")
    val result = countSelfIncluded(input)

    Then("Result is [2]")
    result shouldBe 2
  }

  "countSelfIncluded" should "return result from Puzzle Input" in {
    // GIVEN: input
    Given("Input")
    val input = parse(puzzleInput)

    When("countSelfIncluded(input)")
    val result = countSelfIncluded(input)

    Then("Result is expected")
    result shouldBe 599
  }

  "isOverlapped" should "return false for [2-4,6-8]" in {
    // GIVEN: input
    Given("[2-4,6-8] input")
    val range = RangeDay4(2, 4, 6, 8)

    When("isOverlapped")
    val result = range.isOverlapped

    Then("Result is false")
    result shouldBe false
  }

  "isOverlapped" should "return true for [5-7,7-9]" in {
    // GIVEN: input
    Given("[5-7,7-9] input")
    val range = RangeDay4(5, 7, 7, 9)

    When("isOverlapped")
    val result = range.isOverlapped

    Then("Result is true")
    result shouldBe true
  }

  "isOverlapped" should "return false for [64-67,43-63]" in {
    // GIVEN: input
    Given("[64-67,43-63] input")
    val range = RangeDay4(64,67,43,63)

    When("isOverlapped")
    val result = range.isOverlapped

    Then("Result is false")
    result shouldBe false
  }

  "countOverlapped" should "return result from Spec Input" in {
    // GIVEN: input
    Given("Input")
    val input = parse(puzzleSpecInput)

    When("countOverlapped(input)")
    val result = countOverlapped(input)

    Then("Result is [4]")
    result shouldBe 4
  }

  "countOverlapped" should "return result from Puzzle Input" in {
    // GIVEN: input
    Given("Input")
    val input = parse(puzzleInput)

    When("countOverlapped(input)")
    val result = countOverlapped(input)

    Then("Result is expected")
    result shouldBe 928
  }

}
