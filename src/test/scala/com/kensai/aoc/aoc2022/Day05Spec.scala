package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day05._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readRawInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day05Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readRawInputFile(
    "src/test/resources/2022/Day05.input"
  )

  private lazy val puzzleSpecInput = readRawInputFile(
    "src/test/resources/2022/Day05Spec.input"
  )

  "parse" should "return spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.stacks.get(Point2D(1,1)) shouldBe Some('Z')
    result.stacks.get(Point2D(1,2)) shouldBe Some('N')
    result.stacks.get(Point2D(2,1)) shouldBe Some('M')
    result.stacks.get(Point2D(2,2)) shouldBe Some('C')
    result.stacks.get(Point2D(2,3)) shouldBe Some('D')
    result.stacks.get(Point2D(3,1)) shouldBe Some('P')
    result.stacks should have size 6

    result.instructions.head shouldBe MoveInstruction(1, 2, 1)
    result.instructions(1) shouldBe MoveInstruction(3, 1, 3)
    result.instructions(2) shouldBe MoveInstruction(2, 2, 1)
    result.instructions(3) shouldBe MoveInstruction(1, 1, 2)
    result.instructions should have size 4
  }

  "executePart1(firstStep)" should "return valid state" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executePart1(input)")
    val result = executePart1(input)

    Then("Result is expected")
    result.stacks.get(Point2D(1, 1)) shouldBe Some('Z')
    result.stacks.get(Point2D(1, 2)) shouldBe Some('N')
    result.stacks.get(Point2D(2, 1)) shouldBe Some('M')
    result.stacks.get(Point2D(2, 2)) shouldBe Some('C')
    result.stacks.get(Point2D(1, 3)) shouldBe Some('D')
    result.stacks.get(Point2D(3, 1)) shouldBe Some('P')
    result.stacks should have size 6

    result.instructions.head shouldBe MoveInstruction(3, 1, 3)
    result.instructions(1) shouldBe MoveInstruction(2, 2, 1)
    result.instructions(2) shouldBe MoveInstruction(1, 1, 2)
    result.instructions should have size 3
  }

  "executePart1(secondStep)" should "return valid state" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executePart1(input)")
    val result = executePart1(executePart1(input))

    Then("Result is expected")
    result.stacks.get(Point2D(3, 4)) shouldBe Some('Z')
    result.stacks.get(Point2D(3, 3)) shouldBe Some('N')
    result.stacks.get(Point2D(2, 1)) shouldBe Some('M')
    result.stacks.get(Point2D(2, 2)) shouldBe Some('C')
    result.stacks.get(Point2D(3, 2)) shouldBe Some('D')
    result.stacks.get(Point2D(3, 1)) shouldBe Some('P')
    result.stacks should have size 6

    result.instructions.head shouldBe MoveInstruction(2, 2, 1)
    result.instructions(1) shouldBe MoveInstruction(1, 1, 2)
    result.instructions should have size 2
  }

  "executePart1(thirdStep)" should "return valid state" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executePart1(input)")
    val result = executePart1(executePart1(executePart1(input)))

    Then("Result is expected")
    result.stacks.get(Point2D(3, 4)) shouldBe Some('Z')
    result.stacks.get(Point2D(3, 3)) shouldBe Some('N')
    result.stacks.get(Point2D(1, 2)) shouldBe Some('M')
    result.stacks.get(Point2D(1, 1)) shouldBe Some('C')
    result.stacks.get(Point2D(3, 2)) shouldBe Some('D')
    result.stacks.get(Point2D(3, 1)) shouldBe Some('P')
    result.stacks should have size 6

    result.instructions.head shouldBe MoveInstruction(1, 1, 2)
    result.instructions should have size 1
  }

  "executePart1(fourthStep)" should "return valid state" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executePart1(input)")
    val result = executePart1(executePart1(executePart1(executePart1(input))))

    Then("Result is expected")
    result.stacks.get(Point2D(3, 4)) shouldBe Some('Z')
    result.stacks.get(Point2D(3, 3)) shouldBe Some('N')
    result.stacks.get(Point2D(2, 1)) shouldBe Some('M')
    result.stacks.get(Point2D(1, 1)) shouldBe Some('C')
    result.stacks.get(Point2D(3, 2)) shouldBe Some('D')
    result.stacks.get(Point2D(3, 1)) shouldBe Some('P')
    result.stacks should have size 6

    result.instructions shouldBe empty
  }

  "executeAll(part1)" should "return solution for Spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executeAll(input)")
    val result = executeAll(input, true)

    Then("Result is [CMZ]")
    result shouldBe "CMZ"
  }

  "executeAll(part1)" should "return solution for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("executeAll(input)")
    val result = executeAll(input, true)

    Then("Result is expected")
    result shouldBe "JDTMRWCQJ"
  }

  "executePart2(firstStep)" should "return valid state" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executePart2(input)")
    val result = executePart2(input)

    Then("Result is expected")
    result.stacks.get(Point2D(1, 1)) shouldBe Some('Z')
    result.stacks.get(Point2D(1, 2)) shouldBe Some('N')
    result.stacks.get(Point2D(2, 1)) shouldBe Some('M')
    result.stacks.get(Point2D(2, 2)) shouldBe Some('C')
    result.stacks.get(Point2D(1, 3)) shouldBe Some('D')
    result.stacks.get(Point2D(3, 1)) shouldBe Some('P')
    result.stacks should have size 6

    result.instructions.head shouldBe MoveInstruction(3, 1, 3)
    result.instructions(1) shouldBe MoveInstruction(2, 2, 1)
    result.instructions(2) shouldBe MoveInstruction(1, 1, 2)
    result.instructions should have size 3
  }

  "executePart2(secondStep)" should "return valid state" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executePart2(input)")
    val result = executePart2(executePart2(input))

    Then("Result is expected")
    result.stacks.get(Point2D(3, 2)) shouldBe Some('Z')
    result.stacks.get(Point2D(3, 3)) shouldBe Some('N')
    result.stacks.get(Point2D(2, 1)) shouldBe Some('M')
    result.stacks.get(Point2D(2, 2)) shouldBe Some('C')
    result.stacks.get(Point2D(3, 4)) shouldBe Some('D')
    result.stacks.get(Point2D(3, 1)) shouldBe Some('P')
    result.stacks should have size 6

    result.instructions.head shouldBe MoveInstruction(2, 2, 1)
    result.instructions(1) shouldBe MoveInstruction(1, 1, 2)
    result.instructions should have size 2
  }

  "executeAll(part2)" should "return solution for Spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executeAll(input)")
    val result = executeAll(input, false)

    Then("Result is [MCD]")
    result shouldBe "MCD"
  }

  "executeAll(part2)" should "return solution for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("executeAll(input)")
    val result = executeAll(input, false)

    Then("Result is expected")
    result shouldBe "VHJDDCWRD"
  }

}
