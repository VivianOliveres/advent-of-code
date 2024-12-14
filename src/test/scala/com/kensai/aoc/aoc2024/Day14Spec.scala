package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day14._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day14Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day14.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day14Spec.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse(11, 7, input)")
    val result = parse(11, 7, input)

    Then("Result is Spec Input")
    result.maxX shouldBe 10
    result.maxY shouldBe 6
    result.robots shouldBe Seq(
      Robot(Point2D(0, 0), Point2D(1, 3)),
      Robot(Point2D(2, 0), Point2D(2, -1)),
      Robot(Point2D(3, 0), Point2D(-2, -2)),
      Robot(Point2D(3, 0), Point2D(-1, -2)),
      Robot(Point2D(6, 3), Point2D(-1, -3)),
      Robot(Point2D(7, 3), Point2D(-1, 2)),
      Robot(Point2D(9, 3), Point2D(2, 3)),
      Robot(Point2D(10, 3), Point2D(-1, 2)),
      Robot(Point2D(0, 4), Point2D(3, -3)),
      Robot(Point2D(2, 4), Point2D(2, -3)),
      Robot(Point2D(9, 5), Point2D(-3, -3)),
      Robot(Point2D(7, 6), Point2D(-1, -3)),
    )
  }

  "move(input, robot, 1)" should "work for Point2D(4, 1)" in {
    Given("Spec input")
    val input = parse(11, 7, puzzleSpecInput)
    val robot = Robot(Point2D(2, 4), Point2D(2, -3))

    When("move(input, robot, 1)")
    val result = move(input, robot, 1)

    Then("Result is Point2D(4, 1)")
    result shouldBe Robot(Point2D(4, 1), Point2D(2, -3))
  }

  "move(input, robot, 1)" should "work for Point2D(6, 5)" in {
    Given("Spec input")
    val input = parse(11, 7, puzzleSpecInput)
    val robot = Robot(Point2D(4, 1), Point2D(2, -3))

    When("move(input, robot, 1)")
    val result = move(input, robot, 1)

    Then("Result is Point2D(6, 5)")
    result shouldBe Robot(Point2D(6, 5), Point2D(2, -3))
  }

  "move(input, robot, 1)" should "work for Point2D(8, 2)" in {
    Given("Spec input")
    val input = parse(11, 7, puzzleSpecInput)
    val robot = Robot(Point2D(6, 5), Point2D(2, -3))

    When("move(input, robot, 1)")
    val result = move(input, robot, 1)

    Then("Result is Point2D(8, 2)")
    result shouldBe Robot(Point2D(8, 2), Point2D(2, -3))
  }

  "move(input, robot, 1)" should "work for Point2D(10, 6)" in {
    Given("Spec input")
    val input = parse(11, 7, puzzleSpecInput)
    val robot = Robot(Point2D(8, 2), Point2D(2, -3))

    When("move(input, robot, 1)")
    val result = move(input, robot, 1)

    Then("Result is Point2D(10, 6)")
    result shouldBe Robot(Point2D(10, 6), Point2D(2, -3))
  }

  "move(input, robot, 1)" should "work for Point2D(1, 3)" in {
    Given("Spec input")
    val input = parse(11, 7, puzzleSpecInput)
    val robot = Robot(Point2D(10, 6), Point2D(2, -3))

    When("move(input, robot, 1)")
    val result = move(input, robot, 1)

    Then("Result is Point2D(1, 3)")
    result shouldBe Robot(Point2D(1, 3), Point2D(2, -3))
  }

  "move(input, robot, 5)" should "work" in {
    Given("Spec input")
    val input = parse(11, 7, puzzleSpecInput)
    val robot = Robot(Point2D(2, 4), Point2D(2, -3))

    When("move(input, robot, 5)")
    val result = move(input, robot, 5)

    Then("Result is Point2D(1, 3)")
    result shouldBe Robot(Point2D(1, 3), Point2D(2, -3))
  }

  "safetyFactor(input, 100)" should "find result from puzzle spec input" in {
    Given("Puzzle input")
    val input = parse(11, 7, puzzleSpecInput)

    When("safetyFactor(input, 100)")
    val result = safetyFactor(input, 100)

    Then("Result is expected")
    result shouldBe 12
  }

  "safetyFactor(input, 100)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(101, 103, puzzleInput)

    When("safetyFactor(input, 100)")
    val result = safetyFactor(input, 100)

    Then("Result is expected")
    result shouldBe 215476074
  }

  "print" should "find result from puzzle spec input" in {
    Given("Puzzle input")
    val input = parse(101, 103, puzzleInput)

    When("safetyFactor(input, 100)")
    val result = printUniqueBoard(input)

    Then("Result is expected")
    result shouldBe Some(6285)
  }

}
