package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day01.{ compute, compute2 }
import com.kensai.aoc.lib.Lib.readLongInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day01Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readLongInputLines(
    "src/test/resources/2021/Day01.input"
  )

  private lazy val puzzleSpecInput = readLongInputLines(
    "src/test/resources/2021/Day01Spec.input"
  )

  "compute" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("compute(input)")
    val result = compute(puzzleSpecInput)

    Then("Result is 7")
    result shouldBe 7L
  }

  "compute" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("compute(input)")
    val result = compute(puzzleInput)

    Then("Result is expected")
    result shouldBe 1711L
  }

  "compute2" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("compute2(input)")
    val result = compute2(puzzleSpecInput)

    Then("Result is 5")
    result shouldBe 5L
  }

  "compute2" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("compute2(input)")
    val result = compute2(puzzleInput)

    Then("Result is expected")
    result shouldBe 1743L
  }
}
