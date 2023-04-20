package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day14._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day14Spec extends AnyFlatSpec with GivenWhenThen with Inside {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day14.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day14Spec.input"
  )

  "parse" should "return Inputs for spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.maxX shouldBe 503
    result.minX shouldBe 494
    result.maxY shouldBe 9

    result.points should contain (Point2D(498,4))
    result.points should contain (Point2D(498,5))
    result.points should contain (Point2D(498,6))
    result.points should contain (Point2D(497,6))
    result.points should contain (Point2D(496,6))
    result.points should contain (Point2D(503,4))
    result.points should contain (Point2D(502,4))
    result.points should contain (Point2D(502,5))
    result.points should contain (Point2D(502,6))
    result.points should contain (Point2D(502,7))
    result.points should contain (Point2D(502,8))
    result.points should contain (Point2D(502,9))
    result.points should contain (Point2D(501,9))
    result.points should contain (Point2D(500,9))
    result.points should contain (Point2D(499,9))
    result.points should contain (Point2D(498,9))
    result.points should contain (Point2D(497,9))
    result.points should contain (Point2D(496,9))
    result.points should contain (Point2D(495,9))
    result.points should contain (Point2D(494,9))
    result.points should have size 20
  }

  "addSand" should "work" in {
    // GIVEN: input
    val input = parse(puzzleSpecInput)

    val result1 = addSand(Point2D(500, 0), input)._1
    result1.points should contain allElementsOf(input.points)
    result1.points should contain (Point2D(500, 8))

    val result2 = addSand(Point2D(500, 0), result1)._1
    result2.points should contain allElementsOf(result1.points)
    result2.points should contain (Point2D(499, 8))

    val result3 = addSand(Point2D(500, 0), result2)._1
    result3.points should contain allElementsOf(result2.points)
    result3.points should contain (Point2D(501, 8))

    val result4 = addSand(Point2D(500, 0), result3)._1
    result4.points should contain allElementsOf(result3.points)
    result4.points should contain (Point2D(500, 7))

    val result5 = addSand(Point2D(500, 0), result4)._1
    result5.points should contain allElementsOf(result4.points)
    result5.points should contain (Point2D(498, 8))

    val result6 = addSand(Point2D(500, 0), result5)._1
    result6.points should contain allElementsOf(result5.points)
    result6.points should contain (Point2D(501, 8))

    val result7 = addSand(Point2D(500, 0), result6)._1
    result7.points should contain allElementsOf(result6.points)
    result7.points should contain (Point2D(501, 7))

    val result8 = addSand(Point2D(500, 0), result7)._1
    result8.points should contain allElementsOf(result7.points)
    result8.points should contain (Point2D(500, 6))

    val result9 = addSand(Point2D(500, 0), result8)._1
    result9.points should contain allElementsOf(result8.points)
    result9.points should contain (Point2D(497, 8))

    val result10 = addSand(Point2D(500, 0), result9)._1
    result10.points should contain allElementsOf(result9.points)
    result10.points should contain (Point2D(498, 7))

    val result11 = addSand(Point2D(500, 0), result10)._1
    result11.points should contain allElementsOf(result10.points)
    result11.points should contain (Point2D(499, 6))

    val result12 = addSand(Point2D(500, 0), result11)._1
    result12.points should contain allElementsOf(result11.points)
    result12.points should contain (Point2D(501, 6))

    val result13 = addSand(Point2D(500, 0), result12)._1
    result13.points should contain allElementsOf(result12.points)
    result13.points should contain (Point2D(500, 5))

    val result14 = addSand(Point2D(500, 0), result13)._1
    result14.points should contain allElementsOf(result13.points)
    result14.points should contain (Point2D(499, 5))
  }

  "countSand" should "return 24 for Spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("countSand(input)")
    val result = countSand(Point2D(500, 0), input)

    Then("Result is 24")
    result shouldBe 24
  }

  "countSand" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countSand(input)")
    val result = countSand(Point2D(500, 0), input)

    Then("Result is expected")
    result shouldBe 610
  }

  "countSandV2" should "return 24 for Spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput).part2

    When("countSandV2(input)")
    val result = countSandV2(Point2D(500, 0), input)

    Then("Result is 93")
    result shouldBe 93
  }

  "countSandV2" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput).part2

    When("countSandV2(input)")
    val result = countSandV2(Point2D(500, 0), input)

    Then("Result is expected")
    result shouldBe 27194
  }

}
