package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day09._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day09Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day09.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day09Spec.input"
  )

  "moveAll" should "return new state for every positions of Tail according to Head and for steps between [1,3]" in {
    // 5: Head == Tail
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(0, 0)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(0, 0), Point2D(1, 0), Point2D(2, 0))
    }

    // 6: Head == Tail - 1
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(1, 0)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(1, 0), Point2D(2, 0))
    }

    // 3: Head == Tail - 1 (diag sup)
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(1, 1)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(1, 1), Point2D(2, 0))
    }

    // 9: Head == Tail - 1 (diag inf)
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(1, -1)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(1, -1), Point2D(2, 0))
    }

    // 2: Head == Tail - 1 (top)
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(0, 1)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(0, 1), Point2D(1, 0), Point2D(2, 0))
    }

    // 8: Head == Tail - 1 (bellow)
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(0, -1)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(0, -1), Point2D(1, 0), Point2D(2, 0))
    }

    // 1: Head == Tail - 1 (diag sup bellow)
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(-1, 1)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(-1, 1), Point2D(0, 0), Point2D(1, 0), Point2D(2, 0))
    }

    // 7: Head == Tail - 1 (diag inf bellow)
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(-1, -1)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(-1, -1), Point2D(0, 0), Point2D(1, 0), Point2D(2, 0))
    }

    // 4: Head == Tail - 1
    {
      val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(-1, 0)))
      val np      = moveAll(initPos, MoveInstruction(Right, 3), 2)
      np(0).last shouldBe Point2D(3, 0)
      np(1).last shouldBe Point2D(2, 0)
      np(1).toSet shouldBe Set(Point2D(-1, 0), Point2D(0, 0), Point2D(1, 0), Point2D(2, 0))
    }
  }

  "parse" should "return Forest for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.headPosition shouldBe Point2D(0, 0)
    result.tailPosition shouldBe Point2D(0, 0)
    result.instructions should contain(MoveInstruction(Right, 4))
    result.instructions should contain(MoveInstruction(Up, 4))
    result.instructions should contain(MoveInstruction(Left, 3))
    result.instructions should contain(MoveInstruction(Down, 1))
    result.instructions should contain(MoveInstruction(Right, 4))
    result.instructions should contain(MoveInstruction(Down, 1))
    result.instructions should contain(MoveInstruction(Left, 5))
    result.instructions should contain(MoveInstruction(Right, 2))
    result.instructions should have size 8
  }

  "moveAll(1st Step)" should "return result for [R,4]" in {
    // GIVEN: input
    Given("Instruction [R,4]")
    val instr   = MoveInstruction(Right, 4)
    val initPos = Map(0 -> Seq(Point2D(0, 0)), 1 -> Seq(Point2D(0, 0)))

    When("moveAll(instr)")
    val newPositions = moveAll(initPos, instr, 2)

    Then("Result is expected")
    newPositions(0).last shouldBe Point2D(4, 0)
    newPositions(1).last shouldBe Point2D(3, 0)
    newPositions(1).toSet shouldBe Set(Point2D(0, 0), Point2D(1, 0), Point2D(2, 0), Point2D(3, 0))
  }

  "moveHead(2nd Step)" should "return result for [U,4]" in {
    // GIVEN: input
    Given("Instruction [U,4]")
    val instr   = MoveInstruction(Up, 4)
    val initPos = Map(0 -> Seq(Point2D(4, 0)), 1 -> Seq(Point2D(3, 0)))

    When("moveHead(instr)")
    val newPositions = moveAll(initPos, instr, 2)

    Then("Result is expected")
    newPositions(0).last shouldBe Point2D(4, 4)
    newPositions(1).last shouldBe Point2D(4, 3)
    newPositions(1).toSet shouldBe Set(Point2D(3, 0), Point2D(4, 1), Point2D(4, 2), Point2D(4, 3))
  }

  "moveHead(3rd Step)" should "return result for [L,3]" in {
    // GIVEN: input
    Given("Instruction [L,3]")
    val instr   = MoveInstruction(Left, 3)
    val initPos = Map(0 -> Seq(Point2D(4, 4)), 1 -> Seq(Point2D(4, 3)))

    When("moveHead(instr)")
    val newPositions = moveAll(initPos, instr, 2)

    Then("Result is expected")
    newPositions(0).last shouldBe Point2D(1, 4)
    newPositions(1).last shouldBe Point2D(2, 4)
    newPositions(1).toSet shouldBe Set(Point2D(4, 3), Point2D(3, 4), Point2D(2, 4))
  }

  "moveHead(4th Step)" should "return result for [D,1]" in {
    // GIVEN: input
    Given("Instruction [D,1]")
    val instr   = MoveInstruction(Down, 1)
    val initPos = Map(0 -> Seq(Point2D(1, 4)), 1 -> Seq(Point2D(2, 4)))

    When("moveHead(instr)")
    val newPositions = moveAll(initPos, instr, 2)

    Then("Result is expected")
    newPositions(0).last shouldBe Point2D(1, 3)
    newPositions(1).last shouldBe Point2D(2, 4)
    newPositions(1).toSet shouldBe Set(Point2D(2, 4))
  }

  "moveHead(5th Step)" should "return result for [R,4]" in {
    // GIVEN: input
    Given("Instruction [R,4]")
    val instr   = MoveInstruction(Right, 4)
    val initPos = Map(0 -> Seq(Point2D(1, 3)), 1 -> Seq(Point2D(2, 4)))

    When("moveHead(instr)")
    val newPositions = moveAll(initPos, instr, 2)

    Then("Result is expected")
    newPositions(0).last shouldBe Point2D(5, 3)
    newPositions(1).last shouldBe Point2D(4, 3)
    newPositions(1).toSet shouldBe Set(Point2D(2, 4), Point2D(3, 3), Point2D(4, 3))
  }

  "moveHead(6th Step)" should "return result for [D,1]" in {
    // GIVEN: input
    Given("Instruction [D,1]")
    val instr = MoveInstruction(Down, 1)
    val initPos = Map(0 -> Seq(Point2D(5, 3)), 1 -> Seq(Point2D(4, 3)))

    When("moveHead(instr)")
    val newPositions = moveAll(initPos, instr, 2)

    Then("Result is expected")
    newPositions(0).last shouldBe Point2D(5, 2)
    newPositions(1).last shouldBe Point2D(4, 3)
    newPositions(1).toSet shouldBe Set(Point2D(4, 3))
  }

  "moveHead(7th Step)" should "return result for [L,5]" in {
    // GIVEN: input
    Given("Instruction [L,5]")
    val instr = MoveInstruction(Left, 5)
    val initPos = Map(0 -> Seq(Point2D(5, 2)), 1 -> Seq(Point2D(4, 3)))

    When("moveHead(instr)")
    val newPositions = moveAll(initPos, instr, 2)

    Then("Result is expected")
    newPositions(0).last shouldBe Point2D(0, 2)
    newPositions(1).last shouldBe Point2D(1, 2)
    newPositions(1).toSet shouldBe Set(Point2D(4, 3), Point2D(3, 2), Point2D(2, 2), Point2D(1, 2))
  }

  "executeAll" should "return result for spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("executeAll(input)")
    val newPositions = executeAll(input, 2)

    Then("Result is expected")
    val visitedPositions = newPositions(1)
    visitedPositions should contain(Point2D(0, 0))
    visitedPositions should contain(Point2D(1, 0))
    visitedPositions should contain(Point2D(2, 0))
    visitedPositions should contain(Point2D(3, 0))
    visitedPositions should contain(Point2D(4, 1))
    visitedPositions should contain(Point2D(1, 2))
    visitedPositions should contain(Point2D(2, 2))
    visitedPositions should contain(Point2D(3, 2))
    visitedPositions should contain(Point2D(4, 2))
    visitedPositions should contain(Point2D(3, 3))
    visitedPositions should contain(Point2D(4, 3))
    visitedPositions should contain(Point2D(2, 4))
    visitedPositions should contain(Point2D(3, 4))
    visitedPositions should have size 13
  }

  "countTailPositions(2)" should "return result for spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("countTailPositions(input)")
    val result = countTailPositions(input, 2)

    Then("Result is expected")
    result shouldBe 13
  }

  "countTailPositions(2)" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countTailPositions(input)")
    val result = countTailPositions(input, 2)

    Then("Result is expected")
    result shouldBe 5710
  }

  "moveAll(10)" should "return result for spec input" in {
    val initPos = (0 to 9).map(i => i -> Seq(Point2D(0, 0))).toMap
    val newPositions1 = moveAll(initPos, MoveInstruction(Right, 5), 10)
    newPositions1(0).last shouldBe Point2D(5, 0)
    newPositions1(1).last shouldBe Point2D(4, 0)
    newPositions1(2).last shouldBe Point2D(3, 0)
    newPositions1(3).last shouldBe Point2D(2, 0)
    newPositions1(4).last shouldBe Point2D(1, 0)
    newPositions1(5).last shouldBe Point2D(0, 0)
    newPositions1(6).last shouldBe Point2D(0, 0)
    newPositions1(7).last shouldBe Point2D(0, 0)
    newPositions1(8).last shouldBe Point2D(0, 0)
    newPositions1(9).last shouldBe Point2D(0, 0)

    val newPositions2 = moveAll(newPositions1, MoveInstruction(Up, 8), 10)
    (0 to 9).foreach(i => println(s"$i => ${newPositions2(i)}"))
    newPositions2(0).last shouldBe Point2D(5, 8)
    newPositions2(1).last shouldBe Point2D(5, 7)
    newPositions2(2).last shouldBe Point2D(5, 6)
    newPositions2(3).last shouldBe Point2D(5, 5)
    newPositions2(4).last shouldBe Point2D(5, 4)
    newPositions2(5).last shouldBe Point2D(4, 4)
    newPositions2(6).last shouldBe Point2D(3, 3)
    newPositions2(7).last shouldBe Point2D(2, 2)
    newPositions2(8).last shouldBe Point2D(1, 1)
    newPositions2(9).last shouldBe Point2D(0, 0)
  }

  "countTailPositions(10)" should "return result for spec input" in {
    // GIVEN: input
    Given("spec input")
    val input = parse(puzzleSpecInput)

    When("countTailPositions(input)")
    val result = countTailPositions(input, 10)

    Then("Result is expected")
    result shouldBe 1
  }

  "countTailPositions(10)" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countTailPositions(input)")
    val result = countTailPositions(input, 10)

    Then("Result is expected")
    result shouldBe 2259
  }

}
