package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day10._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day10Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day10.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day10Spec.input"
  )

  "parse" should "return Instructions for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.head shouldBe AddX(15)
    result(1) shouldBe AddX(-11)
    result(2) shouldBe AddX(6)
    result(3) shouldBe AddX(-3)
    result(4) shouldBe AddX(5)
    result(5) shouldBe AddX(-1)
    result(6) shouldBe AddX(-8)
    result(7) shouldBe AddX(13)
    result(8) shouldBe AddX(4)
    result(9) shouldBe Noop
    result should have size 146
  }

  "computeSignalStrength" should "return result for 3 instructions" in {
    // GIVEN: input
    Given("Input is Seq(Noop, AddX(3), AddX(-5))")
    val instructions = Seq(Noop, AddX(3), AddX(-5))

    When("computeSignalStrength(instructions)")
    val signalStrengths = computeSignalStrength(instructions)

    Then("Result is expected")
    signalStrengths(1) shouldBe 1
    signalStrengths(2) shouldBe 1
    signalStrengths(3) shouldBe 1
    signalStrengths(4) shouldBe 4
    signalStrengths(5) shouldBe 4
    signalStrengths(6) shouldBe -1
  }

  "sumSignalStrength" should "return result for puzzle spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("sumSignalStrength(input)")
    val result = sumSignalStrength(input)

    Then("Result is 13140")
    result shouldBe 13140
  }

  "sumSignalStrength" should "return result for input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("sumSignalStrength(input)")
    val result = sumSignalStrength(input)

    Then("Result is expected")
    result shouldBe 14360
  }

  "printImage" should "return result for puzzle spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("printImage(input)")
    val result = printImage(input)

    Then("Result is expected")
    result shouldBe
      """##..##..##..##..##..##..##..##..##..##..
        |###...###...###...###...###...###...###.
        |####....####....####....####....####....
        |#####.....#####.....#####.....#####.....
        |######......######......######......####
        |#######.......#######.......#######.....""".stripMargin
  }

  "printImage" should "return result for spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleInput)

    When("printImage(input)")
    val result = printImage(input)

    Then("Result is expected")
    // BGKAEREZ
    result should be
    """###...##..#..#..##..####.###..####.####.
        |#..#.#..#.#.#..#..#.#....#..#.#.......#.
        |###..#....##...#..#.###..#..#.###....#..
        |#..#.#.##.#.#..####.#....###..#.....#...
        |#..#.#..#.#.#..#..#.#....#.#..#....#....
        |###...###.#..#.#..#.####.#..#.####.####.
        |""".stripMargin
  }

}
