package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day02._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day02Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day02.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day02Spec.input"
  )

  "parse1" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(input")
    val result = parse1(puzzleSpecInput)

    Then("Result is expected")
    result.head shouldBe Round(Rock, Paper)
    result(1) shouldBe Round(Paper, Rock)
    result(2) shouldBe Round(Scissors, Scissors)
    result should have size 3
  }

  "compute1" should "find result from spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val rounds = parse1(puzzleSpecInput)

    When("computeScore(rounds)")
    val result = computeScore(rounds)

    Then("Result is 15")
    result shouldBe 15
  }

  "compute1" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val rounds = parse1(puzzleInput)

    When("computeScore(rounds)")
    val result = computeScore(rounds)

    Then("Result is expected")
    result shouldBe 11449
  }

  "compute2" should "find result from spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val rounds = parse2(puzzleSpecInput)

    When("computeScore(rounds)")
    val result = computeScore(rounds)

    Then("Result is 12")
    result shouldBe 12
  }

  "compute2" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val rounds = parse2(puzzleInput)

    When("computeScore(rounds)")
    val result = computeScore(rounds)

    Then("Result is expected")
    result shouldBe 13187
  }

}
