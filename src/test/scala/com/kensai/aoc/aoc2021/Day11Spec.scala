package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day11._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day11.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day11Spec.input"
  )

  "countFlashes(1)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countFlashes(puzzleSpecInput, 1)")
    val result = countFlashes(puzzleSpecInput, 1)

    Then("Result is 0")
    result shouldBe 0
  }


  "countFlashes(2)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countFlashes(puzzleSpecInput, 2)")
    val result = countFlashes(puzzleSpecInput, 2)

    Then("Result is 35")
    result shouldBe 35
  }

  "countFlashes(3)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countFlashes(puzzleSpecInput, 3)")
    val result = countFlashes(puzzleSpecInput, 3)

    Then("Result is 35+45")
    result shouldBe 35+45
  }

  "countFlashes(100)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countFlashes(puzzleSpecInput, 100)")
    val result = countFlashes(puzzleSpecInput, 100)

    Then("Result is 1656")
    result shouldBe 1656
  }

  "countFlashes(100)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("countFlashes(puzzleInput, 100)")
    val result = countFlashes(puzzleInput, 100)

    Then("Result is expected")
    result shouldBe 1669
  }

  "computeFirstGlobalFlash" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeFirstGlobalFlash(input)")
    val result = computeFirstGlobalFlash(puzzleSpecInput)

    Then("Result is 195")
    result shouldBe 195
  }

  "computeFirstGlobalFlash" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeFirstGlobalFlash(puzzleInput)")
    val result = computeFirstGlobalFlash(puzzleInput)

    Then("Result is expected")
    result shouldBe 351
  }
}
