package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day17._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day17Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day17.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day17Spec.input"
  )

  "computeHighestVelocityY" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput.head

    When("computeHighestVelocityY(puzzleSpecInput)")
    val result = computeHighestVelocityY(input)

    Then("Result is ((6,9), 45)")
    result shouldBe ((6, 9), 45)
  }

  "computeHighestVelocityY" should "find result from input" in {
    Given("Puzzle input")
    val input = puzzleInput.head

    When("computeHighestVelocityY(puzzleInput)")
    val result = computeHighestVelocityY(input)

    Then("Result is expected")
    result shouldBe ((14, 143), 10296)
  }

  "countVelocityY" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput.head

    When("countVelocityY(puzzleSpecInput)")
    val result = countVelocityY(input)

    Then("Result is 112")
    result shouldBe 112
  }

  "countVelocityY" should "find result from input" in {
    Given("Puzzle input")
    val input = puzzleInput.head

    When("countVelocityY(puzzleInput)")
    val result = countVelocityY(input)

    Then("Result is expected")
    // 234 is too low
    result shouldBe 2371
  }

}
