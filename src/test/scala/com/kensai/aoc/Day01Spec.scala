package com.kensai.aoc

import com.kensai.aoc.Day01._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day01Spec extends AnyFlatSpec with GivenWhenThen {

  val ExpectedSum = 2020

  "compute" should "find result from input" in {
    Given("Puzzle input")
    val input = readInputFile("src/test/resources/Day01.input")

    When("compute(2020, input)")
    val result = compute(ExpectedSum, input)

    Then("Result is Some(1007104)")
    result shouldBe Some(1007104)
  }

  "compute3" should "find result from input" in {
    // GIVEN: input from specs
    Given("")
    val input = readInputFile("src/test/resources/Day01.input")

    When("compute3(2020, input)")
    val result = compute3(ExpectedSum, input)

    Then("Result is Some(18847752)")
    result shouldBe Some(18847752)
  }
  private def readInputFile(path: String): List[Long] =
    Source.fromFile(path)
      .getLines
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toLong)
      .toList
}
