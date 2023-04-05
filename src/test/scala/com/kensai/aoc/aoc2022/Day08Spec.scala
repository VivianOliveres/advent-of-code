package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day08._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day08Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day08.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day08Spec.input"
  )

  "parse" should "return Forest for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.maxX shouldBe 4
    result.maxY shouldBe 4
    result.forest shouldBe Map(
      Point2D(0, 0) -> 3,
      Point2D(1, 0) -> 0,
      Point2D(2, 0) -> 3,
      Point2D(3, 0) -> 7,
      Point2D(4, 0) -> 3,
      Point2D(0, 1) -> 2,
      Point2D(1, 1) -> 5,
      Point2D(2, 1) -> 5,
      Point2D(3, 1) -> 1,
      Point2D(4, 1) -> 2,
      Point2D(0, 2) -> 6,
      Point2D(1, 2) -> 5,
      Point2D(2, 2) -> 3,
      Point2D(3, 2) -> 3,
      Point2D(4, 2) -> 2,
      Point2D(0, 3) -> 3,
      Point2D(1, 3) -> 3,
      Point2D(2, 3) -> 5,
      Point2D(3, 3) -> 4,
      Point2D(4, 3) -> 9,
      Point2D(0, 4) -> 3,
      Point2D(1, 4) -> 5,
      Point2D(2, 4) -> 3,
      Point2D(3, 4) -> 9,
      Point2D(4, 4) -> 0
    )
  }

  "countVisibleTrees" should "return result for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("countVisibleTrees(input)")
    val result = countVisibleTrees(input)

    Then("Result is expected")
    result shouldBe 21
  }

  "countVisibleTrees" should "return result for puzzle input" in {
    // GIVEN: input
    Given("puzzle input")
    val input = parse(puzzleInput)

    When("countVisibleTrees(input)")
    val result = countVisibleTrees(input)

    Then("Result is expected")
    result shouldBe 1835
  }

  "treeScenicScore(2, 1)" should "return 4 for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("treeScenicScore(input)")
    val result = treeScenicScore(input, Point2D(2, 1))

    Then("Result is expected")
    result shouldBe 4
  }

  "treeScenicScore(2, 3)" should "return 8 for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("treeScenicScore(input)")
    val result = treeScenicScore(input, Point2D(2, 3))

    Then("Result is expected")
    result shouldBe 8
  }

  "highestTreeScenicScore" should "return result for puzzle input" in {
    // GIVEN: input
    Given("puzzle input")
    val input = parse(puzzleInput)

    When("highestTreeScenicScore(input)")
    val result = highestTreeScenicScore(input)

    Then("Result is expected")
    result shouldBe 263670
  }

}
