package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day04._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day04Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputFile(
    "src/test/resources/2021/Day04.input"
  )

  private lazy val puzzleSpecInput = readInputFile(
    "src/test/resources/2021/Day04Spec.input"
  )

  "computeBestBoardScore" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeBestBoardScore(input)")
    val result = computeBestBoardScore(puzzleSpecInput)

    Then("Result is 4512")
    result shouldBe 4512L
  }

  "computeBestBoardScore" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeBestBoardScore(puzzleInput)")
    val result = computeBestBoardScore(puzzleInput)

    Then("Result is expected")
    result shouldBe 8136L
  }

  "computeWorstBoardScore" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeWorstBoardScore(input)")
    val result = computeWorstBoardScore(puzzleSpecInput)

    Then("Result is 1924")
    result shouldBe 1924L
  }

  "computeWorstBoardScore" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeWorstBoardScore(puzzleInput)")
    val result = computeWorstBoardScore(puzzleInput)

    Then("Result is expected")
    result shouldBe 12738L
  }
}
