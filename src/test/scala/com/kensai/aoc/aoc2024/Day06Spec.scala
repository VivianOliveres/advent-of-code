package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day06._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day06Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day06.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day06Spec.input"
  )

  "parse" should "work for Spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result.maxX shouldBe 9
    result.maxY shouldBe 9
    result.guardPos shouldBe Point2D(4, 6)
    result.guardDirection shouldBe North
    result.obstacles should have size 8
    result.obstacles should contain(Point2D(4, 0))
  }

  "moveGuard" should "should work for spec" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    val input1  = Day6Tmp(input = input, guardPos = input.guardPos, guardDirection = input.guardDirection, path = Set())
    val result1 = moveGuard(input1)
    result1._1.get.guardPos shouldBe Point2D(4, 1)
    result1._1.get.guardDirection shouldBe East
    result1._2.size shouldBe 6

    val input2  = result1._1.get
    val result2 = moveGuard(input2)
    result2._1.get.guardPos shouldBe Point2D(8, 1)
    result2._1.get.guardDirection shouldBe South
    result2._2.size shouldBe 10
  }

  "countGuardPath" should "find result from spec" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("countGuardPath(input)")
    val result = countGuardPath(input)

    Then("Result is 41")
    result shouldBe 41
  }

  "countGuardPath" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countGuardPath(input)")
    val result = countGuardPath(input)

    Then("Result is expected")
    result shouldBe 5444
  }

  "countLoops" should "find result from spec" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("countLoops(input)")
    val result = countLoops(input)

    Then("Result is 6")
    result shouldBe 6
  }

  "countLoops" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countLoops(input)")
    val result = countLoops(input)

    Then("Result is expected")
    result shouldBe 1946
  }

}
