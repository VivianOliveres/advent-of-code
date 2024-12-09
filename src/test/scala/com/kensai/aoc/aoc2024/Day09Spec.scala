package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day09._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day09Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputFile(
    "src/test/resources/2024/Day09.input"
  )

  private lazy val puzzleSpecInput = readInputFile(
    "src/test/resources/2024/Day09Spec.input"
  )

  "parse(12345)" should "work for Spec input" in {
    val input = "12345"

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result.blocks shouldBe Seq((FullId(0), 1), (EmptyId, 2), (FullId(1), 3), (EmptyId, 4), (FullId(2), 5))
  }

  "rearrange(12345)" should "find result from spec" in {
    Given("Puzzle Spec input")
    val input = Day9Input(Seq((FullId(0), 1), (EmptyId, 2), (FullId(1), 3), (EmptyId, 4), (FullId(2), 5)))

    When("rearrange(12345)")
    val result = rearrange(input)

    Then("Result is 022111222")
    result shouldBe Seq((FullId(0), 1), (FullId(2), 2), (FullId(1), 3), (FullId(2), 3))
  }

  "rearrange" should "find result from spec" in {
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("rearrange(input)")
    val result = rearrange(input)

    Then("Result is 0099811188827773336446555566")
    result shouldBe Seq((FullId(0), 2), (FullId(9), 2),(FullId(8), 1), (FullId(1), 3), (FullId(8), 3),
      (FullId(2), 1), (FullId(7), 3), (FullId(3), 3), (FullId(6), 1), (FullId(4), 2), (FullId(6), 1),
      (FullId(5), 4),
      (FullId(6), 1), (FullId(6), 1)) // Double last one: FIXME
  }

  "checkSum" should "find result from Spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("checkSum(input)")
    val result = checkSum(input)

    Then("Result is expected")
    result shouldBe 1928L
  }

  "checkSum" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countAntiNodes(input)")
    val result = checkSum(input)

    Then("Result is expected")
    result shouldBe 6334655979668L
  }

  "rearrange2" should "find result from spec" in {
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("rearrange(input)")
    val result = rearrange3(input)

    Then("Result is 00992111777.44.333....5555.6666.....8888..")
    result shouldBe Seq(
      (FullId(0), 2), (FullId(9), 2),(FullId(2), 1), (FullId(1), 3), (FullId(7), 3),
      (EmptyId, 1),
      (FullId(4), 2),
      (EmptyId, 1),
      (FullId(3), 3),
      (EmptyId, 4),
      (FullId(5), 4),
      (EmptyId, 1),
      (FullId(6), 4),
      (EmptyId, 5),
      (FullId(8), 4),
      (EmptyId, 2)
    )
  }

  "checkSum2" should "find result from Spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("checkSum2(input)")
    val result = checkSum2(input)

    Then("Result is expected")
    result shouldBe 2858L
  }

  "checkSum2" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("checkSum2(input)")
    val result = checkSum2(input)

    Then("Result is expected")
    result shouldBe 6349492251099L
  }

}
