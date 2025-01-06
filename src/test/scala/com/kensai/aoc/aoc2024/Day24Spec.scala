package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day24._
import com.kensai.aoc.lib.Lib.readRawInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day24Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readRawInputFile(
    "src/test/resources/2024/Day24.input"
  )

  private lazy val puzzleSpecInput = readRawInputFile(
    "src/test/resources/2024/Day24Spec.input"
  )

  "parse(puzzleSpecInput)" should "return input puzzle spec" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.state shouldBe Map(
      "x00" -> true,
      "x01" -> true,
      "x02" -> true,
      "y00" -> false,
      "y01" -> true,
      "y02" -> false,
    )
    result.rest shouldBe Map(
      "z00" -> AndGate("x00", "y00"),
      "z01" -> XorGate("x01", "y01"),
      "z02" -> OrGate("x02", "y02"),
    )
  }

  "computeRegistry(puzzleSpecInput)" should "work for puzzle spec" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("computeRegistry(input)")
    val result = computeRegistry(input)

    Then("Result is")
    result shouldBe 4L
  }

  "extract(puzzleInput, t)" should "work for puzzle" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("computeRegistry(input)")
    val result = computeRegistry(input)

    Then("Result is expected")
    result shouldBe 57588078076750L
  }

}
