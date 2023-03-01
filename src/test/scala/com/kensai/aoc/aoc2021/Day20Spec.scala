package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day20._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day20Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day20.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day20Spec.input"
  )

  "parse" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val result = parse(puzzleSpecInput)

    Then("Result is expected")
    result.image.points should have size 25
    result.enhancementAlgorithm should have size 512

    result.image.points(Point2D(0, 0)) shouldBe true
    result.image.points(Point2D(3, 0)) shouldBe true
    result.image.points(Point2D(0, 1)) shouldBe true
    result.image.points(Point2D(2, 1)) shouldBe false
    result.image.points(Point2D(0, 2)) shouldBe true
    result.image.points(Point2D(1, 2)) shouldBe true
    result.image.points(Point2D(4, 2)) shouldBe true
    result.image.points(Point2D(2, 3)) shouldBe true
    result.image.points(Point2D(2, 4)) shouldBe true
    result.image.points(Point2D(3, 4)) shouldBe true
    result.image.points(Point2D(4, 4)) shouldBe true
  }

  "countPixelsLit(2)" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("countPixelsLit(puzzleSpecInput)")
    val result = countPixelsLit(input, 2)

    Then("Result is 35")
    result shouldBe 35
  }

  "countPixelsLit(2)" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("countPixelsLit(puzzleSpecInput)")
    val result = countPixelsLit(input, 2)

    Then("Result is expected")
    result shouldBe 5503
  }

  "countPixelsLit(50)" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("countPixelsLit(puzzleSpecInput)")
    val result = countPixelsLit(input, 50)

    Then("Result is 3351")
    result shouldBe 3351
  }

  "countPixelsLit(50)" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("countPixelsLit(puzzleSpecInput)")
    val result = countPixelsLit(input, 50)

    Then("Result is expected")
    result shouldBe 19156
  }
}
