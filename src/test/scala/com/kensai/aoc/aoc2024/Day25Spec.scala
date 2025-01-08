package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day25._
import com.kensai.aoc.lib.Lib.readRawInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day25Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readRawInputFile(
    "src/test/resources/2024/Day25.input"
  )

  private lazy val puzzleSpecInput = readRawInputFile(
    "src/test/resources/2024/Day25Spec.input"
  )

  "parse(puzzleSpecInput)" should "return input puzzle spec" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.keys shouldBe Seq(
      Schematics(5,0,2,1,3),
      Schematics(4,3,4,0,2),
      Schematics(3,0,2,0,1)
    )
    result.locks shouldBe Seq(
      Schematics(0,5,3,4,3),
      Schematics(1,2,0,5,3)
    )
  }

  "computeUnique(puzzleSpecInput)" should "work for puzzle spec" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("computeUnique(input)")
    val result = computeUnique(input)

    Then("Result is 3")
    result shouldBe 3
  }

  "computeUnique(puzzleInput, t)" should "work for puzzle" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("computeUnique(input)")
    val result = computeUnique(input)

    Then("Result is expected")
    result shouldBe 3439
  }

}
