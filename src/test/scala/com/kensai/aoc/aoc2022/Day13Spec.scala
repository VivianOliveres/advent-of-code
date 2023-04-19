package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day13._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day13Spec extends AnyFlatSpec with GivenWhenThen with Inside {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day13.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day13Spec.input"
  )

  "parse" should "return Inputs for spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result.head shouldBe InputDay13(
      1,
      ListPacket(Seq(ValuePacket(1), ValuePacket(1), ValuePacket(3), ValuePacket(1), ValuePacket(1))),
      ListPacket(Seq(ValuePacket(1), ValuePacket(1), ValuePacket(5), ValuePacket(1), ValuePacket(1)))
    )
    result(1) shouldBe InputDay13(
      2,
      ListPacket(Seq(ListPacket(Seq(ValuePacket(1))), ListPacket(Seq(ValuePacket(2), ValuePacket(3), ValuePacket(4))))),
      ListPacket(Seq(ListPacket(Seq(ValuePacket(1))), ValuePacket(4)))
    )
    result(2) shouldBe InputDay13(
      3,
      ListPacket(Seq(ValuePacket(9))),
      ListPacket(Seq(ListPacket(Seq(ValuePacket(8), ValuePacket(7), ValuePacket(6)))))
    )
    result(3) shouldBe InputDay13(
      4,
      ListPacket(Seq(ListPacket(Seq(ValuePacket(4), ValuePacket(4))), ValuePacket(4), ValuePacket(4))),
      ListPacket(Seq(ListPacket(Seq(ValuePacket(4), ValuePacket(4))), ValuePacket(4), ValuePacket(4), ValuePacket(4)))
    )
    result(4) shouldBe InputDay13(
      5,
      ListPacket(Seq(ValuePacket(7), ValuePacket(7), ValuePacket(7), ValuePacket(7))),
      ListPacket(Seq(ValuePacket(7), ValuePacket(7), ValuePacket(7)))
    )
    result(5) shouldBe InputDay13(6, ListPacket(Seq()), ListPacket(Seq(ValuePacket(3))))
    result(6) shouldBe InputDay13(7, ListPacket(Seq(ListPacket(Seq(ListPacket(Seq()))))), ListPacket(Seq(ListPacket(Seq()))))
    result(7) shouldBe InputDay13(
      8,
      ListPacket(
        Seq(
          ValuePacket(1),
          ListPacket(
            Seq(
              ValuePacket(2),
              ListPacket(
                Seq(ValuePacket(3), ListPacket(Seq(ValuePacket(4), ListPacket(Seq(ValuePacket(5), ValuePacket(6), ValuePacket(7))))))
              )
            )
          ),
          ValuePacket(8),
          ValuePacket(9)
        )
      ),
      ListPacket(
        Seq(
          ValuePacket(1),
          ListPacket(
            Seq(
              ValuePacket(2),
              ListPacket(
                Seq(ValuePacket(3), ListPacket(Seq(ValuePacket(4), ListPacket(Seq(ValuePacket(5), ValuePacket(6), ValuePacket(0))))))
              )
            )
          ),
          ValuePacket(8),
          ValuePacket(9)
        )
      )
    )

    result should have size 8
  }

  "areInRightOrder" should "return true for [1,1,3,1,1] and [1,1,5,1,1]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(1, parseLine("[1,1,3,1,1]"), parseLine("[1,1,5,1,1]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is true")
    result shouldBe true
  }

  "areInRightOrder" should "return true for [[1],[2,3,4]] and [[1],4]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(2, parseLine("[[1],[2,3,4]]"), parseLine("[[1],4]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is true")
    result shouldBe true
  }

  "areInRightOrder" should "return false for [9] and [[8,7,6]]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(3, parseLine("[9]"), parseLine("[[8,7,6]]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is false")
    result shouldBe false
  }

  "areInRightOrder" should "return true for [[4,4],4,4] and [[4,4],4,4,4]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(4, parseLine("[[4,4],4,4]"), parseLine("[[4,4],4,4,4]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is true")
    result shouldBe true
  }

  "areInRightOrder" should "return false for [7,7,7,7] and [7,7,7]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(5, parseLine("[7,7,7,7]"), parseLine("[7,7,7]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is false")
    result shouldBe false
  }

  "areInRightOrder" should "return true for [] and [3]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(6, parseLine("[]"), parseLine("[3]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is true")
    result shouldBe true
  }

  "areInRightOrder" should "return false for [[[]]] and [[]]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(7, parseLine("[[[]]]"), parseLine("[[]]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is false")
    result shouldBe false
  }

  "areInRightOrder" should "return false for [1,[2,[3,[4,[5,6,7]]]],8,9] and [1,[2,[3,[4,[5,6,0]]]],8,9]" in {
    // GIVEN: input
    Given("spec input")
    val input = InputDay13(8, parseLine("[1,[2,[3,[4,[5,6,7]]]],8,9]"), parseLine("[1,[2,[3,[4,[5,6,0]]]],8,9]"))

    When("areInRightOrder(input)")
    val result = areInRightOrder(input)

    Then("Result is false")
    result shouldBe false
  }

  "countRightOrders" should "return result for Spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("countRightOrders(input)")
    val result = countRightOrders(input)

    Then("Result is expected")
    result shouldBe 13
  }

  "countRightOrders" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("countRightOrders(input)")
    val result = countRightOrders(input)

    Then("Result is expected")
    result shouldBe 5555
  }

  "dividerPackets" should "return result for Spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("dividerPackets(input)")
    val result = dividerPackets(input)

    Then("Result is expected")
    result shouldBe 140
  }

  "dividerPackets" should "return result for puzzle input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("dividerPackets(input)")
    val result = dividerPackets(input)

    Then("Result is expected")
    result shouldBe 22852
  }

}
