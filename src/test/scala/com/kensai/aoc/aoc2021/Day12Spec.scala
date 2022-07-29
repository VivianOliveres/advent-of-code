package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day12._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day12Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day12.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day12Spec.input"
  )

  "countUniqPaths" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countUniqPaths(puzzleSpecInput)")
    val result = countUniqPaths(puzzleSpecInput)

    Then("Result is 10")
    result shouldBe 10
  }

  "countUniqPaths" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("countUniqPaths(puzzleInput)")
    val result = countUniqPaths(puzzleInput)

    Then("Result is expected")
    result shouldBe 3576
  }

  "countUniqPaths2" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countUniqPaths2(input)")
    val result = countUniqPaths2(puzzleSpecInput)

    Then("Result is 36")
    result shouldBe 36
  }

  "countUniqPaths2" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("countUniqPaths2(puzzleInput)")
    val result = countUniqPaths2(puzzleInput)

    Then("Result is expected")
    result shouldBe 84271
  }
}
