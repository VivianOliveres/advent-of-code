package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day16._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day16Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day16.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day16Spec.input"
  )

  private lazy val puzzleSpecInput2 = readInputLines(
    "src/test/resources/2024/Day16Spec2.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is Spec Input")
    result.start shouldBe Point2D(1, 13)
    result.startDirection shouldBe East
    result.end shouldBe Point2D(13, 1)
    result.walls should have size (15 + 15 + 13 + 13 + 65)
  }

  "bestScore(input)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("bestScore(input)")
    val result = bestScore(input)

    Then("Result is 7036")
    result shouldBe 7036
  }

  "bestScore(input)" should "find result from puzzle spec input 2" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput2)

    When("bestScore(input)")
    val result = bestScore(input)

    Then("Result is 11048")
    result shouldBe 11048
  }

  "bestScore(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("bestScore(input)")
    val result = bestScore(input)

    Then("Result is expected")
    result shouldBe 102504
  }

  "bestSeats(input)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("bestSeats(input)")
    val result = bestSeats(input)

    Then("Result is 45")
    result shouldBe 45
  }

  "bestSeats(input)" should "find result from puzzle spec input 2" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput2)

    When("bestSeats(input)")
    val result = bestSeats(input)

    Then("Result is 64")
    result shouldBe 64
  }

  "bestSeats(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("bestSeats(input)")
    val result = bestSeats(input)

    Then("Result is expected")
    result shouldBe 535
  }

}
