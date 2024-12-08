package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day08._
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
    "src/test/resources/2024/Day08.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day08Spec.input"
  )

  "parse" should "work for Spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result.maxX shouldBe 11
    result.maxY shouldBe 11
    result.antennas should have size 2
    result.antennas('A') shouldBe Set(Point2D(6, 5), Point2D(8, 8), Point2D(9, 9))
    result.antennas('0') shouldBe Set(Point2D(8, 1), Point2D(5, 2), Point2D(7, 3), Point2D(4, 4))
  }

  "computeAntinodes(1 antenas)" should "return nothing" in {
    When("computeAntiNodes")
    val result = computeAntiNodes(maxX = 11, maxY = 11, antennas = Set(Point2D(5, 5)))

    Then("Result is empty")
    result shouldBe empty
  }

  "computeAntinodes(2 antennas)" should "return 3 anti-nodes" in {
    When("computeAntiNodes")
    val result = computeAntiNodes(maxX = 11, maxY = 11, antennas = Set(Point2D(4, 3), Point2D(5, 5)))

    Then("Result is 2 nodes")
    result shouldBe Set(Point2D(3, 1), Point2D(6, 7))
  }

  "computeAntiNodes(3 antennas)" should "return 4 anti-nodes" in {
    When("computeAntiNodes")
    val result = computeAntiNodes(maxX = 11, maxY = 11, antennas = Set(Point2D(4, 3), Point2D(5, 5), Point2D(8, 4)))

    Then("Result is 4 nodes")
    result shouldBe Set(Point2D(3, 1), Point2D(6, 7), Point2D(0, 2), Point2D(2, 6), Point2D(11, 3))
  }

  "computeAntiNodes(A)" should "return 5 anti-nodes" in {
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("computeAntiNodes")
    val result = computeAntiNodes(maxX = input.maxX, maxY = input.maxY, antennas = input.antennas('A'))

    Then("Result is 5 nodes")
    result shouldBe Set(Point2D(7, 7), Point2D(10, 10), Point2D(10, 11), Point2D(4, 2), Point2D(3, 1))
  }

  "computeAntiNodes(0)" should "return 10 anti-nodes" in {
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("computeAntiNodes")
    val result = computeAntiNodes(maxX = input.maxX, maxY = input.maxY, antennas = input.antennas('0'))

    Then("Result is 10 nodes")
    result should have size 10
  }

  "countAntiNodes" should "find result from spec" in {
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("countAntiNodes(input)")
    val result = countAntiNodes(input)

    Then("Result is 14")
    result shouldBe 14
  }

  "countAntiNodes" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countAntiNodes(input)")
    val result = countAntiNodes(input)

    Then("Result is expected")
    result shouldBe 285
  }

  "computeResonantAntiNodes(T)" should "return 10 anti-nodes" in {
    Given("T antennas")
    val antennas = Set(Point2D(0, 0), Point2D(3, 1), Point2D(1, 2))

    When("computeAntiNodes")
    val result = computeResonantAntiNodes(maxX = 9, maxY = 9, antennas = antennas)

    Then("Result is 9 nodes")
    result should have size 9
    result shouldBe Set(
      Point2D(0, 0), Point2D(3, 1), Point2D(1, 2), // Initial antennas
      Point2D(2, 4),Point2D(3, 6),Point2D(4, 8),
      Point2D(6, 2),Point2D(9, 3),
      Point2D(5, 0),
    )
  }

  "countResonantAntiNodes" should "find result from spec" in {
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("countResonantAntiNodes(input)")
    val result = countResonantAntiNodes(input)

    Then("Result is 34")
    result shouldBe 34
  }

  "countResonantAntiNodes" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countResonantAntiNodes(input)")
    val result = countResonantAntiNodes(input)

    Then("Result is expected")
    result shouldBe 944
  }

}
