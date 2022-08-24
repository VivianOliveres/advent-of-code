package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day18._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day18Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day18.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day18Spec.input"
  )

  "add [[[[4,3],4],4],[7,[[8,4],9]]] and [1,1]" should "produce [[[[0,7],4],[[7,8],[6,0]]],[8,1]]" in {
    Given("[[[[4,3],4],4],[7,[[8,4],9]]] and [1,1]")
    val input1 = parse("[[[[4,3],4],4],[7,[[8,4],9]]]")
    val input2 = parse("[1,1]")

    When("add")
    val result = add(input1, input2)

    Then("Result is [[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    val expected = parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    result shouldBe expected
  }

  "addAll" should "work" in {
    val init = Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]").map(parse)
    val initOutput = addAll(init)
    val initExpected = parse("[[[[1,1],[2,2]],[3,3]],[4,4]]")
    initOutput shouldBe initExpected

    val step1 = add(initOutput, Parent(5, 5))
    val expectedStep1 = parse("[[[[3,0],[5,3]],[4,4]],[5,5]]")
    step1 shouldBe expectedStep1

    val step2 = add(step1, Parent(6, 6))
    val expectedStep2 = parse("[[[[5,0],[7,4]],[5,5]],[6,6]]")
    step2 shouldBe expectedStep2
  }

  "addAll" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = puzzleSpecInput.map(parse)

    When("addAll(puzzleSpecInput)")
    val result = addAll(input)

    Then(
      "Result is [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
    )
    val expectedResult =
      parse("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
    result shouldBe expectedResult

    Then("Magnitude is 4140")
    computeMagnitude(result) shouldBe 4140L
  }

  "computeMagnitude" should "find result from input" in {
    Given("Puzzle input")
    val input = puzzleInput.map(parse)

    When("computeMagnitude(puzzleInput)")
    val resultNode = addAll(input)
    val result = computeMagnitude(resultNode)

    Then("Result is expected")
    result shouldBe 4202L
  }

  "computeLargestMagnitude" should "find result from input" in {
    Given("Puzzle input")
    val input = puzzleInput.map(parse)

    When("computeMagnitude(puzzleInput)")
    val result = computeLargestMagnitude(input)

    Then("Result is expected")
    result shouldBe 4779L
  }

}
