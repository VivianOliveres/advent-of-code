package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day21._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day21Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day21.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day21Spec.input"
  )

  "complexity(input)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("complexity(input)")
    val result = complexity(input)

    Then("Result is 126384")
    result shouldBe 126384
  }

  "complexity(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = puzzleInput

    When("complexity(input)")
    val result = complexity(input)

    Then("Output is expected")
    result shouldBe 157230
  }

  "complexity2(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = puzzleInput

    When("complexity(input)")
    val result = complexity2(input)

    Then("Output is expected")
    result shouldBe 195969155897936L
  }


}
