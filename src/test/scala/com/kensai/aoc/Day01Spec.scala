package com.kensai.aoc

import com.kensai.aoc.Day01._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day01Spec extends AnyFlatSpec with GivenWhenThen {

  val ExpectedSum = 2020

  private lazy val puzzleInput = readLongInputLines("src/test/resources/Day01.input")

  "compute" should "find result from input" in {
    Given("Puzzle input")

    When("compute(2020, input)")
    val result = compute(ExpectedSum, puzzleInput)

    Then("Result is Some(1007104)")
    result shouldBe Some(1007104)
  }

  "compute3" should "find result from input" in {
    // GIVEN: input from specs
    Given("")

    When("compute3(2020, input)")
    val result = compute3(ExpectedSum, puzzleInput)

    Then("Result is Some(18847752)")
    result shouldBe Some(18847752)
  }
}
