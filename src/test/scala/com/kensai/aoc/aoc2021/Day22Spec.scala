package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day22._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day22Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day22.input"
  )

  private lazy val puzzleSpecPart1Input = readInputLines(
    "src/test/resources/2021/Day22SpecPart1.input"
  )

  private lazy val puzzleSpecPart2Input = readInputLines(
    "src/test/resources/2021/Day22SpecPart2.input"
  )

  "parse" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val result = parse(puzzleSpecPart1Input)

    Then("Result is expected")
    result.instructions should have size 22
    result.instructions.filter(_.isOn) should have size 17
    result.instructions.filterNot(_.isOn) should have size 5
  }

  "countCubeOn" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecPart1Input)

    When("countCubeOn(puzzleSpecInput)")
    val result = countCubeOn(input)

    Then("Result is 590784")
    result shouldBe 590784
  }

  "countCubeOn" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("countCubeOn(puzzleSpecInput)")
    val result = countCubeOn(input)

    Then("Result is expected")
    result shouldBe 607657
  }


  "countCubeOn2" should "find result from spec2 input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecPart2Input)

    When("countCubeOn2(puzzleSpecInput)")
    val result = countCubeOn2(input)

    Then("Result is 2758514936282235")
    result shouldBe 2758514936282235L
  }

  "countCubeOn2" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("countCubeOn2(puzzleSpecInput)")
    val result = countCubeOn2(input)

    Then("Result is expected")
    result shouldBe 1187742789778677L
  }
}
