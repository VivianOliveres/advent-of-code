package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day19._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day19Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day19.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day19Spec.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is Spec Input")
    result.available should have size 8
    result.available should contain("r")
    result.available should contain("wr")
    result.available should contain("b")
    result.available should contain("g")
    result.available should contain("bwu")
    result.available should contain("rb")
    result.available should contain("gb")
    result.available should contain("br")

    result.desired should have size 8
    result.desired should contain("brwrr")
  }

  "isPossible(brwrr)" should "be true" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("isPossibe(brwrr)")
    val result = isPossible(input.available, "brwrr")

    Then("Result is true")
    result shouldBe true
  }

  "isPossible(bggr)" should "be true" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("isPossibe(bggr)")
    val result = isPossible(input.available, "bggr")

    Then("Result is true")
    result shouldBe true
  }

  "isPossible(gbbr)" should "be true" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("isPossibe(gbbr)")
    val result = isPossible(input.available, "gbbr")

    Then("Result is true")
    result shouldBe true
  }

  "isPossible(rrbgbr)" should "be true" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("isPossibe(rrbgbr)")
    val result = isPossible(input.available, "rrbgbr")

    Then("Result is true")
    result shouldBe true
  }

  "isPossible(ubwu)" should "be false" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("isPossibe(ubwu)")
    val result = isPossible(input.available, "ubwu")

    Then("Result is false")
    result shouldBe false
  }

  "countPossible(input)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("countPossible(input)")
    val result = countPossible(input)

    Then("Output is 6")
    result shouldBe 6
  }

  "isPossible(wwggwbwruubbwurrgrggbuuwwwgwbrwubggurbrwugguruggggbrwggbbrg)" should "be " in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("isPossibe(wwggwbwruubbwurrgrggbuuwwwgwbrwubggurbrwugguruggggbrwggbbrg)")
    val result = isPossible(input.available, "wwggwbwruubbwurrgrggbuuwwwgwbrwubggurbrwugguruggggbrwggbbrg")

    Then("Result is false")
    println(result)
  }

  "countPossible(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countPossible(input)")
    val result = countPossible(input)

    Then("Output is expected")
    result shouldBe 336
  }

  "sumPossible(input)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("sumPossible(input)")
    val result = sumPossible(input)

    Then("Output is 16")
    result shouldBe 16L
  }

  "sumPossible(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("sumPossible(input)")
    val result = sumPossible(input)

    Then("Output is expected")
    result shouldBe 758890600222015L
  }


}
