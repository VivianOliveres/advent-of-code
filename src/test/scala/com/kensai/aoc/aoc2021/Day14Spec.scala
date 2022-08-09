package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day14._
import com.kensai.aoc.lib.Lib.{readInputLines}
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day14Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day14.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day14Spec.input"
  )

  "parse inputs" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val result = parse(puzzleSpecInput)

    Then("Result is parsed")
    result.polymer should have size 3
    result.rules should have size 16
  }

  "insert(step=1)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("insert(puzzleSpecInput, 1)")
    val inputs = parse(puzzleSpecInput)
    val result = insert(inputs, 1)

    Then("Result is parsed")
    //NCNBCHB
    result shouldBe Map(
      ('N', 'C') -> 1,
      ('C', 'N') -> 1,
      ('N', 'B') -> 1,
      ('B', 'C') -> 1,
      ('C', 'H') -> 1,
      ('H', 'B') -> 1
    )
  }

  "insert(step=2)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("insert(puzzleSpecInput, 2)")
    val inputs = parse(puzzleSpecInput)
    val result = insert(inputs, 2)

    Then("Result is parsed")
    //NBCCNBBBCBHCB
    result(('H', 'C')) shouldBe 1
    result(('C', 'B')) shouldBe 2
    result(('B', 'H')) shouldBe 1
    result(('N', 'B')) shouldBe 2
    result(('B', 'C')) shouldBe 2
    result(('B', 'B')) shouldBe 2
    result(('C', 'N')) shouldBe 1
    result(('C', 'C')) shouldBe 1
    result should have size 8
  }

  "insert(step=3)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("insert(puzzleSpecInput, 3)")
    val inputs = parse(puzzleSpecInput)
    val result = insert(inputs, 3)

    Then("Result is parsed")
    // NBBBCNCCNBBNBNBBCHBHHBCHB
    result(('B', 'N')) shouldBe 2
    result(('N', 'C')) shouldBe 1
    result(('B', 'H')) shouldBe 1
    result(('H', 'H')) shouldBe 1
    result(('N', 'B')) shouldBe 4
    result(('B', 'C')) shouldBe 3
    result(('B', 'B')) shouldBe 4
    result(('C', 'C')) shouldBe 1
    result(('C', 'H')) shouldBe 2
    result(('H', 'B')) shouldBe 3
    result(('C', 'N')) shouldBe 2
    result should have size 11
  }

  "insert(step=4)" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("insert(puzzleSpecInput, 4)")
    val inputs = parse(puzzleSpecInput)
    val result = insert(inputs, 4)

    Then("Result is parsed")
    //NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
    result(('C', 'B')) shouldBe 5
    result(('B', 'N')) shouldBe 6
    result(('N', 'C')) shouldBe 1
    result(('B', 'H')) shouldBe 3
    result(('H', 'H')) shouldBe 1
    result(('N', 'B')) shouldBe 9
    result(('B', 'B')) shouldBe 9
    result(('C', 'N')) shouldBe 3
    result(('C', 'C')) shouldBe 2
    result(('H', 'C')) shouldBe 3
    result(('N', 'H')) shouldBe 1
    result(('H', 'N')) shouldBe 1
    result(('B', 'C')) shouldBe 4
    result should have size 13
  }

  "computeSolution(step=10)" should "find result from spec input" in {
    Given("Puzzle input")

    When("fold(puzzleInput)")
    val inputs = parse(puzzleSpecInput)
    val result = computeSolution(inputs, 10)

    Then("Result is 1588")
    result shouldBe 1588
  }

  "computeSolution(step=10)" should "find result from input" in {
    Given("Puzzle input")

    When("fold(puzzleInput)")
    val inputs = parse(puzzleInput)
    val result = computeSolution(inputs, 10)

    Then("Result is expected")
    result shouldBe 2712
  }

  "computeSolution(step=40)" should "find result from input" in {
    Given("Puzzle input")

    When("fold(puzzleInput)")
    val inputs = parse(puzzleInput)
    val result = computeSolution(inputs, 40)

    Then("Result is expected")
    result shouldBe 8336623059567L
  }
}
