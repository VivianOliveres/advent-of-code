package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day13._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day13Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputFile(
    "src/test/resources/2021/Day13.input"
  )

  private lazy val puzzleSpecInput = readInputFile(
    "src/test/resources/2021/Day13Spec.input"
  )

  "parse inputs" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val result = parse(puzzleSpecInput)

    Then("Result is parsed")
    result.dots should have size 18
    result.folds should have size 2
    result.folds shouldBe Seq(YFold(7), XFold(5))
  }

  "fold" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val inputs = parse(puzzleSpecInput)
    val result = foldAll(Inputs(inputs.dots, Seq(inputs.folds.head)))

    Then("Result is parsed")
    result should have size 17
  }

  "fold" should "find result from input" in {
    Given("Puzzle input")

    When("fold(puzzleInput)")
    val inputs = parse(puzzleInput)
    val result = foldAll(Inputs(inputs.dots, Seq(inputs.folds.head)))

    Then("Result is expected")
    result should have size 814
  }

  "foldAll" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("foldAll(puzzleInput)")
    val inputs = parse(puzzleInput)
    val result = foldAll(inputs)

    Then("Result is expected")
    result should have size 108

    // Solution is "pzehraer"
    printSolution(result)
  }
}
