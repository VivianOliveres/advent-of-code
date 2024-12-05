package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day05._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day05Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputFile(
    "src/test/resources/2024/Day05.input"
  )

  private lazy val puzzleSpecInput = readInputFile(
    "src/test/resources/2024/Day05Spec.input"
  )

  "parse" should "work for Spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput

    When("parse")
    val result = parse(input)

    Then("Result is Spec Input")
    result.rules should not be empty
    result.rules should contain(29 -> Seq(13))
    result.updates shouldBe Seq(
      Seq(75, 47, 61, 53, 29),
      Seq(97, 61, 53, 29, 13),
      Seq(75, 29, 13),
      Seq(75, 97, 47, 61, 53),
      Seq(61, 13, 29),
      Seq(97, 13, 75, 29, 47)
    )
  }

  "filterValidRules(spec)" should "return 3 values" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("filterValidRules(input)")
    val result = filterRules(input, true)

    Then("Result is 3")
    result shouldBe Seq(
      Seq(75, 47, 61, 53, 29),
      Seq(97, 61, 53, 29, 13),
      Seq(75, 29, 13)
    )
  }

  "validateUpdate" should "work" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("validateUpdate")
    val result = validateUpdate(input.rules, Seq(61, 13, 29), 0, Seq())

    Then("Result is false")
    result shouldBe false
  }

  "sumValidMidPages" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("sumValidMidPages(input)")
    val result = sumValidMidPages(input)

    Then("Result is 143")
    result shouldBe 143
  }

  "sumValidMidPages" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("sumValidMidPages(input)")
    val result = sumValidMidPages(input)

    Then("Result is expected")
    result shouldBe 6384
  }

  "reorder(61,13,29)" should "produce (61,29,13)" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("reorder")
    val result = reorder(input.rules, Seq(61, 13, 29))

    Then("Result is (61,29,13)")
    result shouldBe Seq(61, 29, 13)
  }

  "reorder(75,97,47,61,53)" should "produce (97,75,47,61,53)" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("reorder")
    val result = reorder(input.rules, Seq(75, 97, 47, 61, 53))

    Then("Result is (97,75,47,61,53)")
    result shouldBe Seq(97, 75, 47, 61, 53)
  }

  "reorder(97,13,75,29,47)" should "produce (97,75,47,29,13)" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("reorder")
    val result = reorder(input.rules, Seq(97, 13, 75, 29, 47))

    Then("Result is (97,75,47,29,13)")
    result shouldBe Seq(97, 75, 47, 29, 13)
  }

  "sumReorderedInvalidMidPages" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("sumReorderedInvalidMidPages(input)")
    val result = sumReorderedInvalidMidPages(input)

    Then("Result is 123")
    result shouldBe 123
  }

  "sumReorderedInvalidMidPages" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("sumReorderedInvalidMidPages(input")
    val result = sumReorderedInvalidMidPages(input)

    Then("Result is expected")
    result shouldBe 5353
  }

}
