package com.kensai.aoc

import com.kensai.aoc.Day08._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day08Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputLines("src/test/resources/Day08.input")
  private lazy val specInputs = readInputLines("src/test/resources/Day08Spec.input")

  "accumulatorBeforeLoop for spec" should "return 5" in {
    Given(s"Input is spec")

    When(s"accumulatorBeforeLoop(input)")
    val result = accumulatorBeforeLoop(specInputs)

    Then(s"Result is 5")
    result shouldBe 5
  }

  "accumulatorBeforeLoop for puzzle" should "return solution" in {
    Given(s"Puzzle input")

    When(s"accumulatorBeforeLoop(input)")
    val result = accumulatorBeforeLoop(puzzleInputs)

    Then(s"Result is 2003")
    result shouldBe 2003
  }

  "accumulatorAfterFixingInputs for spec" should "return 8" in {
    Given(s"Input is spec")

    When(s"accumulatorAfterFixingInputs(input)")
    val result = accumulatorAfterFixingInputs(specInputs)

    Then(s"Result is 8")
    result shouldBe 8
  }

  "accumulatorAfterFixingInputs for puzzle" should "return solution" in {
    Given(s"Puzzle solution")

    When(s"accumulatorAfterFixingInputs(input)")
    val result = accumulatorAfterFixingInputs(puzzleInputs)

    Then(s"Result is 1984")
    result shouldBe 1984
  }

}
