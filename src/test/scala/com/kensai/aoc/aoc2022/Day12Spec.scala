package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day12._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day12Spec extends AnyFlatSpec with GivenWhenThen with Inside {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day12.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day12Spec.input"
  )

  "parse" should "return Monkeys for spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result(Point2D(0, 0)) shouldBe StartCell
    result(Point2D(1, 0)) shouldBe ValueCell('a')
    result(Point2D(2, 0)) shouldBe ValueCell('b')
    result(Point2D(3, 0)) shouldBe ValueCell('q')
    result(Point2D(4, 0)) shouldBe ValueCell('p')
    result(Point2D(5, 0)) shouldBe ValueCell('o')
    result(Point2D(6, 0)) shouldBe ValueCell('n')
    result(Point2D(7, 0)) shouldBe ValueCell('m')
    result(Point2D(0, 1)) shouldBe ValueCell('a')
    result(Point2D(1, 1)) shouldBe ValueCell('b')
    result(Point2D(2, 1)) shouldBe ValueCell('c')
    result(Point2D(3, 1)) shouldBe ValueCell('r')
    result(Point2D(4, 1)) shouldBe ValueCell('y')
    result(Point2D(5, 1)) shouldBe ValueCell('x')
    result(Point2D(6, 1)) shouldBe ValueCell('x')
    result(Point2D(7, 1)) shouldBe ValueCell('l')
    result(Point2D(5, 2)) shouldBe EndCell
    result should have size 40
  }

  "computeShortestPath" should "return result for puzzle spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("computeShortestPath(input)")
    val result = computeShortestPath(input)

    Then("Result is 31")
    result shouldBe 31
  }

  "computeShortestPath" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("computeShortestPath(input)")
    val result = computeShortestPath(input)

    Then("Result is expected")
    result shouldBe 528
  }


  "computeAnyShortestPath" should "return result for puzzle spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("computeAnyShortestPath(input)")
    val result = computeAnyShortestPath(input)

    Then("Result is 29")
    result shouldBe 29
  }

  "computeAnyShortestPath" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("computeAnyShortestPath(input)")
    val result = computeAnyShortestPath(input)

    Then("Result is expected")
    result shouldBe 522
  }


}
