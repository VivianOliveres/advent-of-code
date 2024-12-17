package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day17._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day17Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day17.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day17Spec.input"
  )


  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is Spec Input")
    result.a shouldBe 729L
    result.b shouldBe 0L
    result.c shouldBe 0L
    result.program shouldBe Seq(0L,1L,5L,4L,3L,0L)
    result.output shouldBe empty
    result.operationPointer shouldBe 0
  }

  "executeInput(2,6)" should "work" in {
    Given("Puzzle spec input")
    val input = Day17Input(0L, 0L, 9L, Seq(2L, 6L), Seq(), 0)

    When("executeInput(input)")
    val result = executeInput(input)

    Then("B is 1")
    result.b shouldBe 1
  }

  "executeInput(5,0,5,1,5,4)" should "work" in {
    Given("Puzzle spec input")
    val input = Day17Input(10L, 0L, 0L, Seq(5L,0L,5L,1L,5L,4L), Seq(), 0)

    When("executeInput(input)")
    val result = executeInput(input)

    Then("output is 0, 1, 2")
    result.output shouldBe Seq(0L, 1L, 2L)
  }

  "executeInput(0,1,5,4,3,0)" should "work" in {
    Given("Puzzle spec input")
    val input = Day17Input(2024L, 0L, 0L, Seq(0L,1L,5L,4L,3L,0L), Seq(), 0)

    When("executeInput(input)")
    val result = executeInput(input)

    Then("output is 4,2,5,6,7,7,7,7,3,1,0")
    result.a shouldBe 0
    result.output shouldBe Seq(4L,2L,5L,6L,7L,7L,7L,7L,3L,1L,0L)
  }

  "executeInput(1,7)" should "work" in {
    Given("Puzzle spec input")
    val input = Day17Input(0L, 29L, 0L, Seq(1L,7L), Seq(), 0)

    When("executeInput(input)")
    val result = executeInput(input)

    Then("B is 26")
    result.b shouldBe 26L
  }

  "executeInput(4,0)" should "work" in {
    Given("Puzzle spec input")
    val input = Day17Input(0L, 2024L, 43690L, Seq(4L,0L), Seq(), 0)

    When("executeInput(input)")
    val result = executeInput(input)

    Then("B is 44354")
    result.b shouldBe 44354L
  }

  "executeInput(input)" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("executeInput(input)")
    val result = executeInput(input)

    Then("Output is 4,6,3,5,6,3,5,2,1,0")
    result.output shouldBe Seq(4L,6L,3L,5L,6L,3L,5L,2L,1L,0L)
  }

  "executeInput(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("executeInput(input)")
    val result = executeInput(input)

    Then("Output is expected")
    result.output shouldBe Seq(1L,5L,0L,3L,7L,3L,0L,3L,1L)
  }

  // TODO: part2

//  "outputItself(input)" should "find result from puzzle spec input" in {
//    Given("Puzzle spec input")
//    val input = Day17Input(2024L, 0L, 0L, Seq(0L,3L,5L,4L,3L,0L), Seq(), 0)
//
//    When("outputItself(input)")
//    val result = outputItself(input)
//
//    Then("Result is 117440")
//    result shouldBe 117440L
//  }
//
//  "outputItself(input)" should "find result from puzzle input" in {
//    Given("Puzzle input")
//    val input = parse(puzzleInput)
//
//    When("outputItself(input)")
//    val result = outputItself(input)
//
//    Then("Result is expected")
//    result shouldBe 1L
//  }


}
