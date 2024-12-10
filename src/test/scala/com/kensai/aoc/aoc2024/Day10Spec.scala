package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day10._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day10Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day10.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day10Spec.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result.maxX shouldBe 7
    result.maxY shouldBe 7
    result.trailHeads.sortBy(point => (point.y, point.x)) shouldBe Seq(
      Point2D(2, 0), Point2D(4, 0), Point2D(4, 2), Point2D(6, 4),
      Point2D(2, 5), Point2D(5, 5), Point2D(0, 6), Point2D(6, 6),
      Point2D(1, 7)
    ).sortBy(point => (point.y, point.x))
    result.grid should have size 64
  }

  "hikingScore(2, 0)" should "be 5" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("hikingScore(input)")
    val result = hikingScore(input, Point2D(2, 0))

    Then("Result is 5")
    result shouldBe 5
  }

  "hikingScore" should "find result from Spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("hikingScore(input)")
    val result = hikingScore(input)

    Then("Result is expected")
    result shouldBe 36
  }

  "hikingScore" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("hikingScore(input)")
    val result = hikingScore(input)

    Then("Result is expected")
    result shouldBe 617
  }

  "hikingRating(2, 0)" should "be 5" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("hikingRating(input)")
    val result = hikingRating(input, Point2D(2, 0))

    Then("Result is 20")
    result shouldBe 20
  }

  "hikingRating(4, 0)" should "be 5" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("hikingRating(input)")
    val result = hikingRating(input, Point2D(4, 0))

    Then("Result is 24")
    result shouldBe 24
  }

  "hikingRating" should "find result from Spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("hikingRating(input)")
    val result = hikingRating(input)

    Then("Result is expected")
    result shouldBe 81
  }

  "hikingRating" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("hikingRating(input)")
    val result = hikingRating(input)

    Then("Result is expected")
    result shouldBe 1477
  }

}
