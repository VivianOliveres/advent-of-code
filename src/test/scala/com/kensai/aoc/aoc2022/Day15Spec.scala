package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day15._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15Spec extends AnyFlatSpec with GivenWhenThen with Inside {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day15.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day15Spec.input"
  )

  "parse" should "return Sensors for spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val results = parse(input)

    Then("Result is expected")
    results should contain(Sensor(Point2D(2, 18), Point2D(-2, 15)))
    results should contain(Sensor(Point2D(9, 16), Point2D(10, 16)))
    results should contain(Sensor(Point2D(13, 2), Point2D(15, 3)))
    results should contain(Sensor(Point2D(12, 14), Point2D(10, 16)))
    results should contain(Sensor(Point2D(10, 20), Point2D(10, 16)))
    results should contain(Sensor(Point2D(14, 17), Point2D(10, 16)))
    results should contain(Sensor(Point2D(8, 7), Point2D(2, 10)))
    results should contain(Sensor(Point2D(2, 0), Point2D(2, 10)))
    results should contain(Sensor(Point2D(0, 11), Point2D(2, 10)))
    results should contain(Sensor(Point2D(20, 14), Point2D(25, 17)))
    results should contain(Sensor(Point2D(17, 20), Point2D(21, 22)))
    results should contain(Sensor(Point2D(16, 7), Point2D(15, 3)))
    results should contain(Sensor(Point2D(14, 3), Point2D(15, 3)))
    results should contain(Sensor(Point2D(20, 1), Point2D(15, 3)))
  }

  "countInvalidPositions(10)" should "return 13 for Sensor((8, 7), (2, 10))" in {
    // GIVEN: input
    Given("Sensor((8, 7), (2, 10))")
    val input = Seq(Sensor(Point2D(8, 7), Point2D(2, 10)))

    When("countInvalidPositions(10)")
    val result = countInvalidPositions(input, 10)

    Then("Result is 12")
    result shouldBe 12
  }

  "countInvalidPositions(10)" should "return 26 for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("countInvalidPositions(10)")
    val result = countInvalidPositions(input, 10)

    Then("Result is 26")
    result shouldBe 26
  }

  "countInvalidPositions(2000000)" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countInvalidPositions(input, 2000000)")
    val result = countInvalidPositions(input, 2000000)

    Then("Result is expected")
    result shouldBe 5240818
  }

  "findValidPosition(row[10], max[20])" should "return None for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("findValidPosition(10, 20)")
    val result = findValidPosition(input, 10, 20)

    Then("Result is None")
    result shouldBe None
  }

  "findValidPosition(row[11], max[20])" should "return Some(14,11) for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("findValidPosition(10, 20)")
    val result = findValidPosition(input, 11, 20)

    Then("Result is (14,11)")
    result shouldBe Some(Point2D(14, 11))
  }

  "tuningFrequency(max[20])" should "return 56000011 for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("tuningFrequency(20)")
    val result = tuningFrequency(input, 20)

    Then("Result is 56000011")
    result shouldBe 56000011L
  }

  "tuningFrequency(max[4000000])" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("tuningFrequency(input, 4000000)")
    val result = tuningFrequency(input, 4000000)

    Then("Result is expected")
    result shouldBe 13213086906101L
  }

}
