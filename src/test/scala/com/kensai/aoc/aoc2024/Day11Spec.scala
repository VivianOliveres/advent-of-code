package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day11._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day11.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day11Spec.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result shouldBe Seq(125L, 17L)
  }

  "blink(0L, 1L, 10L, 99L, 999L)" should "return (1L, 2024L, 1L, 0L, 9L, 9L, 2021976L)" in {
    val input = Seq(0L, 1L, 10L, 99L, 999L)

    When("blink")
    val result = blink(input, 1)

    Then("Result is (1 2024 1 0 9 9 2021976)")
    result shouldBe 7L
  }

  "blink(spec)" should "work for Spec input" in {
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("blink")
    val result1 = blink(input, 1)
    Then("Result 1 is (253000 1 7)")
    result1 shouldBe 3L

    When("blink")
    val result2 = blink(input, 2)
    Then("Result 2 is (253L, 0L, 2024L, 14168L)")
    result2 shouldBe 4L

    When("blink")
    val result3 = blink(input, 3)
    Then("Result 3 is (512072 1 20 24 28676032)")
    result3 shouldBe 5L
  }

  "blink 6 times" should "find result from Spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("blink(6, input)")
    val result = blink(input, 6)

    Then("Result is expected")
    result shouldBe 22L
  }

  "blink 25 times" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("blink(25, input)")
    val result = blink(input, 25)

    Then("Result is expected")
    result shouldBe 185894L
  }

  "blink 75 times" should "find result from input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("blink(25, input)")
    val result = blink(input, 75)

    Then("Result is expected")
    result shouldBe 221632504974231L
  }


}
