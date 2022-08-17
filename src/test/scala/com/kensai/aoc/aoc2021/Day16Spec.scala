package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day16._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day16Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day16.input"
  )

  "computeSumVersionNumbers" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(hexToBin(puzzleInput.head))

    When("computeSumVersionNumbers(puzzleInput)")
    val result = computeSumVersionNumbers(input)

    Then("Result is expected")
    result shouldBe 821
  }

  "eval" should "find result for input" in {
    Given("Puzzle input")
    val input = parse(hexToBin(puzzleInput.head))

    When("eval(puzzleInput)")
    val result = input.eval

    Then("Result is expected")
    result shouldBe 2056021084691L
  }

}
