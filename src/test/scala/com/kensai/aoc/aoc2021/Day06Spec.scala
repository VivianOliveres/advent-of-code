package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day06._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day06Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputFile(
    "src/test/resources/2021/Day06.input"
  )

  private lazy val puzzleSpecInput = readInputFile(
    "src/test/resources/2021/Day06Spec.input"
  )

  "computeFishesCount(80)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeFishesCount(input, 80)")
    val result = computeFishesCount(puzzleSpecInput, 80)

    Then("Result is 5934")
    result shouldBe 5934L
  }

  "computeFishesCount(80)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeFishesCount(puzzleInput, 80)")
    val result = computeFishesCount(puzzleInput, 80)

    Then("Result is expected")
    result shouldBe 390923L
  }

  "computeFishesCount(256)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeFishesCount(input, 256)")
    val result = computeFishesCount(puzzleSpecInput, 256)

    Then("Result is 26984457539")
    result shouldBe 26984457539L
  }

  "computeFishesCount(256)" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeFishesCount(puzzleInput)")
    val result = computeFishesCount(puzzleInput, 256)

    Then("Result is expected")
    result shouldBe 1749945484935L
  }
}
