package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day03._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day03Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputFile(
    "src/test/resources/2024/Day03.input"
  )

  private lazy val puzzleSpecInput = readInputFile(
    "src/test/resources/2024/Day03Spec.input"
  )

  "parse(mul(44,46))" should "return Reports" in {
    Given("Input: mul(44,46)")
    val input = "mul(44,46)"

    When("parse(input)")
    val result = parseMul(input)

    Then("Result is 6 reports of 5 levels")
    result shouldBe Some(Mul(44, 46))
  }

  "parseAll(Spec)" should "return Reports" in {
    Given("Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parseAllMul(input)

    Then("Result is 6 reports of 5 levels")
    result should have size 4
    result should contain (Mul(2, 4))
    result should contain (Mul(5, 5))
    result should contain (Mul(11, 8))
    result should contain (Mul(8, 5))
  }

  "executeAll" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parseAllMul(puzzleSpecInput)

    When("countSafe(input)")
    val result = executeAll(input)

    Then("Result is 161")
    result shouldBe 161
  }

  "executeAll" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parseAllMul(puzzleInput)

    When("executeAll(input")
    val result = executeAll(input)

    Then("Result is expected")
    result shouldBe 184122457
  }

  "parseAllCommands(Spec)" should "return Reports" in {
    Given("Spec input")
    val input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    When("parseAllCommands(input)")
    val result = parseAllCommands(input)

    Then("Result is 6 reports of 5 levels")
    result should have size 6
    result should contain (Mul(2, 4))
    result should contain (Dont)
    result should contain (Mul(5, 5))
    result should contain (Mul(11, 8))
    result should contain (Do)
    result should contain (Mul(8, 5))
  }

  "executeAllCommands" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parseAllCommands("\"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))\"")

    When("executeAllCommands(input)")
    val result = executeAllCommands(input)

    Then("Result is 48")
    result shouldBe 48
  }

  "executeAllCommands" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parseAllCommands(puzzleInput)

    When("executeAllCommands(input")
    val result = executeAllCommands(input)

    Then("Result is expected")
    result shouldBe 107862689
  }

}
