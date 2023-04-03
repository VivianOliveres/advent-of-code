package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day24._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day24Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day24.input"
  )

  "parse" should "find steps from input" in {
    Given("Puzzle spec input")

    When("parse(puzzleInput)")
    val steps = parse(puzzleInput)

    Then("Result is expected")
    steps.head shouldBe Step(1, 12, 4)
    steps(1) shouldBe Step(1, 15, 11)
    steps should have size(14)
  }

  "findLargestModelNumber" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("findLargestModelNumber(puzzleInput)")
    val result = findLargestModelNumber(input)

    Then("Result is expected")
    result shouldBe 92928914999991L
  }

  "findSmallestModelNumber" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("findSmallestModelNumber(puzzleSpecInput)")
    val result = findSmallestModelNumber(input)

    Then("Result is expected")
    result shouldBe 91811211611981L
  }

}
