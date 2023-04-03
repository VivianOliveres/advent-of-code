package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day25._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day25Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day25.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day25Spec.input"
  )

  "parse" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parsePart1(puzzleSpecInput)")
    val result = parse(puzzleSpecInput)

    Then("Result is expected")
    result.stepNumber shouldBe 0
    result.maxX shouldBe 9
    result.maxY shouldBe 8

    result.eastSeaCuCumber should contain(Point2D(4, 0))
    result.eastSeaCuCumber should contain(Point2D(5, 0))
    result.eastSeaCuCumber should contain(Point2D(9, 0))
    result.eastSeaCuCumber should contain(Point2D(3, 1))
    result.eastSeaCuCumber should contain(Point2D(4, 1))

    result.southSeaCuCumber should contain(Point2D(0, 0))
    result.southSeaCuCumber should contain(Point2D(7, 0))
    result.southSeaCuCumber should contain(Point2D(8, 0))
  }

  "executeStep (from initial step)" should "return step 1" in {
    Given("Initial step")
    val input = InputDay25(
      stepNumber = 0,
      eastSeaCuCumber = Set(
        Point2D(3, 0),
        Point2D(6, 2),
        Point2D(6, 3),
        Point2D(6, 4)
      ),
      southSeaCuCumber = Set(
        Point2D(0, 3),
        Point2D(2, 6),
        Point2D(3, 6),
        Point2D(4, 6)
      ),
      maxX = 6,
      maxY = 6
    )

    When("executeStep(input)")
    val result = executeStep(input)

    Then("Result is expected")
    result.stepNumber shouldBe 1
    result.maxX shouldBe 6
    result.maxY shouldBe 6

    result.eastSeaCuCumber should have size 4
    result.eastSeaCuCumber should contain(Point2D(4, 0))
    result.eastSeaCuCumber should contain(Point2D(0, 2))
    result.eastSeaCuCumber should contain(Point2D(6, 3))
    result.eastSeaCuCumber should contain(Point2D(0, 4))
    result.southSeaCuCumber should have size 4
    result.southSeaCuCumber should contain(Point2D(2, 0))
    result.southSeaCuCumber should contain(Point2D(3, 0))
    result.southSeaCuCumber should contain(Point2D(0, 3))
    result.southSeaCuCumber should contain(Point2D(4, 6))
  }

  "computeLastStepWithoutAnythingMoving" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("computeLastStepWithoutAnythingMoving(puzzleSpecInput)")
    val result = computeLastStepWithoutAnythingMoving(input)

    Then("Result is 58")
    result shouldBe 58
  }

  "computeLastStepWithoutAnythingMoving" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("computeLastStepWithoutAnythingMoving(puzzleSpecInput)")
    val result = computeLastStepWithoutAnythingMoving(input)

    Then("Result is expected")
    result shouldBe 308
  }
}
