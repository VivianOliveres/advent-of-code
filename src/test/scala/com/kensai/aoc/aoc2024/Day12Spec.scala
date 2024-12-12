package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day12._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day12Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day12.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day12Spec.input"
  )

  private lazy val puzzleSpecInput2 = readInputLines(
    "src/test/resources/2024/Day12Spec2.input"
  )

  private lazy val puzzleSpecInput3 = readInputLines(
    "src/test/resources/2024/Day12Spec3.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result.maxX shouldBe 9
    result.maxY shouldBe 9
    result.gardenPlots should have size 100
    result.gardenPlots(Point2D(0, 0)) shouldBe 'R'
    result.gardenPlots(Point2D(9, 9)) shouldBe 'E'
  }

  "doCompute(A)" should "work for Spec input" in {
    val input = parse(puzzleSpecInput2)

    When("doCompute")
    val (region, visited) = doCompute(input, Point2D(0, 0), Set(), None)

    Then("Result is Spec Input")
    region shouldBe Region('A', 4, 10)
    visited shouldBe Set(Point2D(0, 0), Point2D(1, 0), Point2D(2, 0), Point2D(3, 0))
  }

  "computeRegions" should "work for Spec input" in {
    val input = parse(puzzleSpecInput2)

    When("computeRegions")
    val result = computeRegions(input)

    Then("Result is Spec Input")
    result should have size 5
    result.sortBy(_.id) shouldBe Seq(
      Region('A', 4, 10),
      Region('B', 4, 8),
      Region('C', 4, 10),
      Region('D', 1, 4),
      Region('E', 3, 8)
    )
  }

  "sumPrice" should "work for Spec input" in {
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("sumPrice")
    val result = sumPrice(input)

    Then("Result is 1930")
    result shouldBe 1930
  }

  "sumPrice" should "find result from Spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("sumPrice(input)")
    val result = sumPrice(input)

    Then("Result is expected")
    result shouldBe 1477924
  }

  "compute(A)" should "work for Spec input" in {
    val input = parse(puzzleSpecInput2)

    When("compute")
    val result = compute(input, Set(Point2D(0, 0), Point2D(1, 0), Point2D(2, 0), Point2D(3, 0)))

    Then("Result is Spec Input")
    result shouldBe Region2('A', 4, 4)
  }

  "compute(B)" should "work for Spec input" in {
    val input = parse(puzzleSpecInput2)

    When("compute")
    val result = compute(input, Set(Point2D(0, 1), Point2D(1, 1), Point2D(0, 2), Point2D(1, 2)))

    Then("Result is Spec Input")
    result shouldBe Region2('B', 4, 4)
  }

  "computeRegions2" should "work for Spec input" in {
    val input = parse(puzzleSpecInput2)

    When("parse")
    val result = computeRegions2(input)

    Then("Result is Spec Input")
    result should have size 5
    result.sortBy(_.id) shouldBe Seq(
      Region2('A', 4, 4),
      Region2('B', 4, 4),
      Region2('C', 4, 8),
      Region2('D', 1, 4),
      Region2('E', 3, 4)
    )
  }

  "computeRegions2" should "work for Spec input 3" in {
    val input = parse(puzzleSpecInput3)

    When("parse")
    val result = computeRegions2(input)

    Then("Result is Spec Input")
    result should have size 3
    result.sortBy(_.id) shouldBe Seq(
      Region2('A', 28, 12),
      Region2('B', 4, 4),
      Region2('B', 4, 4)
    )
  }

  "countCorners(0,0)" should "work for Spec input 3" in {
    val input = parse(puzzleSpecInput3)

    When("parse")
    val regionA = extractRegions(input).find(_.contains(Point2D(0, 0))).get
    val result = countCorners(input, regionA, List(Point2D(0, 0)), 0)

    Then("Result is Spec Input")
    result shouldBe 1
  }

  "countCorners(1,0)" should "work for Spec input 3" in {
    val input = parse(puzzleSpecInput3)

    When("parse")
    val regionA = extractRegions(input).find(_.contains(Point2D(0, 0))).get
    val result = countCorners(input, regionA, List(Point2D(1, 0)), 0)

    Then("Result is Spec Input")
    result shouldBe 0
  }

  "sumPrice2" should "work for Spec input" in {
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("sumPrice2")
    val result = sumPrice2(input)

    Then("Result is 1206")
    result shouldBe 1206
  }

  "sumPrice2" should "find result from Spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("sumPrice2(input)")
    val result = sumPrice2(input)

    Then("Result is expected")
    result shouldBe 841934
  }

}
