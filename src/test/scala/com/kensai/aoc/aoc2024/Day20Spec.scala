package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day20._
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
    "src/test/resources/2024/Day20.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day20Spec.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is Spec Input")
    result.maxX shouldBe 14
    result.maxY shouldBe 14
    result.start shouldBe Point2D(1, 3)
    result.end shouldBe Point2D(5, 7)
//    result.walls should have size ???
  }

  "initialPath(input)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("initialPath(input)")
    val result = initialPath(input)

    Then("Result is 84")
    result should have size 84
  }

  "countUnder100Cheating(input, 2)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("countUnder100Cheating(input)")
    val result = countUnder100Cheating(input, 2)

    Then("Result is 0")
    result shouldBe 0
  }

  //TODO: improve: 1,5 sec
  "countUnder100Cheating(input, 2)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countUnder100Cheating(input)")
    val result = countUnder100Cheating(input, 2)

    Then("Output is expected")
    result shouldBe 1490
  }

  "countUnder100Cheating(input, 20)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("countUnder100Cheating(input)")
    val result = countUnder100Cheating(input, 20)

    Then("Result is 0")
    result shouldBe 0
  }

  // TODO: Improve: 45 sec
  "countUnder100Cheating(input, 20)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countUnder100Cheating(input)")
    val result = countUnder100Cheating(input, 20)

    Then("Output is expected")
    result shouldBe 1011325
  }

}
