package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day19._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day19Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputFile(
    "src/test/resources/2021/Day19.input"
  )

  private lazy val puzzleSpecInput = readInputFile(
    "src/test/resources/2021/Day19Spec.input"
  )

  "parse" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val result = parse(puzzleSpecInput)

    Then("Result is expected")
    result.scanners should have size 5
    result.scanners.head.points should have size 25
  }

  "parse" should "find result from input" in {
    Given("Puzzle input")

    When("parse(puzzleInput)")
    val result = parse(puzzleInput)

    Then("Result is expected")
    result.scanners should have size 35
    result.scanners.head.points should have size 26
  }

  "compute" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("compute(puzzleSpecInput)")
    val result = compute(input)

    Then("Result is 79")
    result shouldBe 79
  }

  "compute" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("compute(puzzleSpecInput)")
    val result = compute(input)

    Then("Result is expected")
    result shouldBe 432
  }

  "Manhattan distance" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("manhattanDistance(puzzleSpecInput)")
    val result = manhattanDistance(input)

    Then("Result is 3621")
    result shouldBe 3621
  }

  "Manhattan distance" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("manhattanDistance(puzzleSpecInput)")
    val result = manhattanDistance(input)

    Then("Result is expected")
    result shouldBe 14414
  }

}
