package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day18._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day18Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day18.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day18Spec.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input, 6)

    Then("Result is Spec Input")
    result.max shouldBe 6
    result.bytesToFall.head shouldBe Point2D(5, 4)
    result.bytesToFall should have size 25
  }

  "minSetpsAfter(input, 12)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput, 6)

    When("minSetpsAfter(input, 12)")
    val result = minStepsAfter(input, 12)

    Then("Output is 22")
    result shouldBe Some(22)
  }

  "minSetpsAfter(input, 1024)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput, 70)

    When("minSetpsAfter(input, 1024)")
    val result = minStepsAfter(input, 1024)

    Then("Output is expected")
    result shouldBe Some(408)
  }

  "minStepsAfterRemoval(input, 12)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput, 6)

    When("minStepsAfterRemoval(input, 12)")
    val result = minStepsAfterRemoval(input, 12)

    Then("Output is (6, 1)")
    result shouldBe Point2D(6, 1)
  }

  "minStepsAfterRemoval(input, 1024)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput, 70)

    When("minStepsAfterRemoval(input, 1024)")
    val result = minStepsAfterRemoval(input, 1024)

    Then("Output is expected")
    result shouldBe Point2D(45, 16)
  }

}
