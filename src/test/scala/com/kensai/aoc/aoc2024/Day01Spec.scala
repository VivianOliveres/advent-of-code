package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day01._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day01Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day01.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day01Spec.input"
  )

  "parse(\"3   4\")" should "return Seq((3, 4))" in {
    Given("Row \"3   4\"")
    val row = puzzleSpecInput.take(1)

    When("parse(row)")
    val result = parse(row)

    Then("Result is (Seq(3), Seq(4))")
    result shouldBe (Seq(3L), Seq(4L))
  }

  "shortestTotalDistance" should "find result from spec input" in {
    Given("Puzzle spec input")
    val parsed = parse(puzzleSpecInput)

    When("shortestTotalDistance(input)")
    val result = shortestTotalDistance(parsed)

    Then("Result is 11")
    result shouldBe 11L
  }

  "shortestTotalDistance" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val parsed = parse(puzzleInput)

    When("shortestTotalDistance(input")
    val result = shortestTotalDistance(parsed)

    Then("Result is expected")
    result shouldBe 1222801L
  }

  "similarityScore" should "find result from spec input" in {
    Given("Puzzle spec input")
    val parsed = parse(puzzleSpecInput)

    When("similarityScore(input)")
    val result = similarityScore(parsed)

    Then("Result is 31")
    result shouldBe 31L
  }

  "similarityScore" should "find result from input" in {
    Given("Puzzle input")
    val parsed = parse(puzzleInput)

    When("similarityScore(input)")
    val result = similarityScore(parsed)

    Then("Result is expected")
    result shouldBe 22545250L
  }
}
