package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day11._
import com.kensai.aoc.lib.Lib.readRawInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Spec extends AnyFlatSpec with GivenWhenThen with Inside {

  private lazy val puzzleInput = readRawInputFile(
    "src/test/resources/2022/Day11.input"
  )

  private lazy val puzzleSpecInput = readRawInputFile(
    "src/test/resources/2022/Day11Spec.input"
  )

  "parse" should "return Monkeys for spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    inside(result(0)) { case Monkey(id, itemsInspected, worryLevels, action) =>
      id shouldBe 0
      itemsInspected shouldBe 0
      worryLevels shouldBe Seq(79, 98)
      action.update(1) shouldBe 19
      action.divideTest shouldBe 23
      action.trueSend shouldBe 2
      action.falseSend shouldBe 3
    }
    inside(result(1)) { case Monkey(id, itemsInspected, worryLevels, action) =>
      id shouldBe 1
      itemsInspected shouldBe 0
      worryLevels shouldBe Seq(54, 65, 75, 74)
      action.update(1) shouldBe 7
      action.divideTest shouldBe 19
      action.trueSend shouldBe 2
      action.falseSend shouldBe 0
    }
    inside(result(2)) { case Monkey(id, itemsInspected, worryLevels, action) =>
      id shouldBe 2
      itemsInspected shouldBe 0
      worryLevels shouldBe Seq(79, 60, 97)
      action.update(2) shouldBe 4
      action.divideTest shouldBe 13
      action.trueSend shouldBe 1
      action.falseSend shouldBe 3
    }
    inside(result(3)) { case Monkey(id, itemsInspected, worryLevels, action) =>
      id shouldBe 3
      itemsInspected shouldBe 0
      worryLevels shouldBe Seq(74)
      action.update(2) shouldBe 5
      action.divideTest shouldBe 17
      action.trueSend shouldBe 0
      action.falseSend shouldBe 1
    }
    result should have size 4
  }

  "executeRound(3, 1st)" should "return new step" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executeRound(input)")
    val result = executeRound(_ / 3, input)

    Then("Result is expected")
    result(0).itemsInspected shouldBe 2
    result(0).worryLevels shouldBe Seq(20, 23, 27, 26)
    result(1).itemsInspected shouldBe 4
    result(1).worryLevels shouldBe Seq(2080, 25, 167, 207, 401, 1046)
    result(2).itemsInspected shouldBe 3
    result(2).worryLevels shouldBe Seq()
    result(3).itemsInspected shouldBe 5
    result(3).worryLevels shouldBe Seq()
    result should have size 4
  }

  "executeRound(3, 2nd)" should "return new step" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executeRound(input)")
    val result = executeRound(_ / 3, executeRound(_ / 3, input))

    Then("Result is expected")
    result(0).worryLevels shouldBe Seq(695, 10, 71, 135, 350)
    result(1).worryLevels shouldBe Seq(43, 49, 58, 55, 362)
    result(2).worryLevels shouldBe Seq()
    result(3).worryLevels shouldBe Seq()
    result should have size 4
  }

  "executeRound(3, 20)" should "return new step" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = parse(puzzleSpecInput)

    When("executeRound(input)")
    val result = (0 until 20).foldLeft(input) { case (i, _) => executeRound(_ / 3, i) }

    Then("Result is expected")
    result(0).worryLevels shouldBe Seq(10, 12, 14, 26, 34)
    result(0).itemsInspected shouldBe 101
    result(1).worryLevels shouldBe Seq(245, 93, 53, 199, 115)
    result(1).itemsInspected shouldBe 95
    result(2).worryLevels shouldBe Seq()
    result(2).itemsInspected shouldBe 7
    result(3).worryLevels shouldBe Seq()
    result(3).itemsInspected shouldBe 105
    result should have size 4
  }

  "computeMonkeyBusiness(20, Part1)" should "return result for puzzle spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("computeMonkeyBusiness(20, 3)")
    val result = computeMonkeyBusiness(20, true, input)

    Then("Result is 10605")
    result shouldBe 10605L
  }

  "computeMonkeyBusiness(20, Part1)" should "return result for input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("computeMonkeyBusiness(20)")
    val result = computeMonkeyBusiness(20, true, input)

    Then("Result is expected")
    result shouldBe 55930L
  }

  "computeMonkeyBusiness(10000, Part2)" should "return result for puzzle spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("computeMonkeyBusiness(10000, 1)")
    val result = computeMonkeyBusiness(10000, false, input)

    Then("Result is 2713310158")
    result shouldBe 2713310158L
  }

  "computeMonkeyBusiness(10000, Part2)" should "return result for input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("computeMonkeyBusiness(10000, 1)")
    val result = computeMonkeyBusiness(10000, false, input)

    Then("Result is expected")
    result shouldBe 14636993466L
  }

}
