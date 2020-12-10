package com.kensai.aoc

import com.kensai.aoc.Day03._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day03Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputLines("src/test/resources/Day03.input")
  private lazy val specInputs = readInputLines("src/test/resources/Day03Spec.input")

  "countTrees(3, 1) for specs" should "return 7" in {
    Given(s"Input is from specs")
    val input = parse(specInputs)

    When("countTrees(3, 1, input)")
    val result = countTrees(3, 1, input)

    Then("Result is 7")
    result shouldBe 7
  }

  "countTrees(3, 1)" should "return solution" in {
    Given(s"Puzzle input")
    val input = parse(puzzleInputs)

    When("countTrees(3, 1, input)")
    val result = countTrees(3, 1, input)

    Then("Result is 299")
    result shouldBe 299
  }

  "countMultipleTrees for spec" should "return solution" in {
    Given(s"Input is from specs")
    val input = parse(specInputs)

    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    When(s"countMultipleTrees($slopes, input)")
    val result = countMultipleTrees(slopes, input)

    Then("Result is 336")
    result shouldBe 336L
  }

  "countMultipleTrees" should "return solution" in {
    Given(s"Puzzle input")
    val input = parse(puzzleInputs)

    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    When(s"countMultipleTrees($slopes, input)")
    val result = countMultipleTrees(slopes, input)

    Then("Result is 3621285278")
    result shouldBe 3621285278L
  }


}
