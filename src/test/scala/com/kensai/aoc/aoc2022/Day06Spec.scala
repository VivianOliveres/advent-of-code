package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day06._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day06Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day06.input"
  )

  "startPacketMarker" should "return solution for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = puzzleInput.head

    When("startPacketMarker")
    val result = startPacketMarker(input)

    Then("Result is expected")
    result shouldBe 1282
  }

  "startMessageMarker" should "return solution for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = puzzleInput.head

    When("startMessageMarker")
    val result = startMessageMarker(input)

    Then("Result is expected")
    result shouldBe 3513
  }


}
