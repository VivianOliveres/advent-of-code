package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day02._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day02Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day02.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day02Spec.input"
  )

  "computePosition" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computePosition(input)")
    val result = computePosition(puzzleSpecInput)

    Then("Result is 150")
    result shouldBe 150
  }

  "computePosition" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computePosition(puzzleInput)")
    val result = computePosition(puzzleInput)

    Then("Result is expected")
    result shouldBe 1868935
  }

  "computePosition2" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("compute2(input)")
    val result = computePosition2(puzzleSpecInput)

    Then("Result is 900")
    result shouldBe 900
  }

  "computePosition2" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computePosition2(puzzleInput)")
    val result = computePosition2(puzzleInput)

    Then("Result is expected")
    result shouldBe 1965970888
  }
}
