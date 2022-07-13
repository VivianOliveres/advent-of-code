package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day05._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day05Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day05.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day05Spec.input"
  )

  "countDangerousPoints" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countDangerousPoints(input)")
    val result = countVHDangerousPoints(puzzleSpecInput)

    Then("Result is 5")
    result shouldBe 5
  }

  "countDangerousPoints" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("countDangerousPoints(puzzleInput)")
    val result = countVHDangerousPoints(puzzleInput)

    Then("Result is expected")
    result shouldBe 5147
  }

  "Generate diagonale" should "create expected points" in {
    Given("Puzzle spec input")
    val line = VentLine(0, 8, 8, 0)

    When("generatePoints")
    val result = line.generatePoints

    Then("Result is expected")
    result shouldBe Seq(
      VentPoint(0, 8),
      VentPoint(1, 7),
      VentPoint(2, 6),
      VentPoint(3, 5),
      VentPoint(4, 4),
      VentPoint(5, 3),
      VentPoint(6, 2),
      VentPoint(7, 1),
      VentPoint(8, 0)
    )
  }

  "countAllDangerousPoints" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("countAllDangerousPoints(input)")
    val result = countAllDangerousPoints(puzzleSpecInput)

    Then("Result is 12")
    result shouldBe 12
  }

  "countAllDangerousPoints" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("countAllDangerousPoints(puzzleInput)")
    val result = countAllDangerousPoints(puzzleInput)

    Then("Result is expected")
    result shouldBe 16925
  }
}
