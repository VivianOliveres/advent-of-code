package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day03._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day03Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day03.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day03Spec.input"
  )

  "sumPrioritiesByElf" should "find result from spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("sumPrioritiesByElf(input)")
    val result = sumPrioritiesByElf(input)

    Then("Result is 157")
    result shouldBe 157
  }

  "sumPrioritiesByElf" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = puzzleInput

    When("sumPrioritiesByElf(input)")
    val result = sumPrioritiesByElf(input)

    Then("Result is expected")
    result shouldBe 7428
  }

  "findSharedItem(elf1, elf2, elf3)" should "find result from input1" in {
    // GIVEN: input
    Given("Input")
    val input = Seq(
      "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg"
    )

    When("findSharedItem(input)")
    val result = findSharedItem(input)

    Then("Result is [r]")
    result shouldBe 'r'
  }

  "findSharedItem(elf1, elf2, elf3)" should "find result from input2" in {
    // GIVEN: input
    Given("Input")
    val input = Seq(
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    )

    When("findSharedItem(input)")
    val result = findSharedItem(input)

    Then("Result is [Z]")
    result shouldBe 'Z'
  }

  "sumPrioritiesByGroup by group" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = puzzleInput

    When("sumPrioritiesByGroup(input)")
    val result = sumPrioritiesByGroup(input)

    Then("Result is expected")
    result shouldBe 2650
  }

}
