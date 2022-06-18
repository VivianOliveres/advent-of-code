package com.kensai.aoc.aoc2021

import Day06._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day06Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputFile(
    "src/test/resources/2021//Day06.input"
  )
  private lazy val specInputs = readInputFile(
    "src/test/resources/2021//Day06Spec.input"
  )

  "part1: sumDifAnswers from specs" should "return 11" in {
    Given("Spec input")

    When(s"sumDifAnswers(input)")
    val result = sumDifAnswers(specInputs)

    Then(s"Result is 11")
    result shouldBe 11
  }

  "part1: sumDifAnswers" should "return solution" in {
    Given("Puzzle input")

    When(s"sumDifAnswers(input)")
    val result = sumDifAnswers(puzzleInputs)

    Then(s"Result is 6443")
    result shouldBe 6443
  }

  "part2: sumEveryoneAnswers from spec" should "return 6" in {
    Given("Spec input")

    When(s"sumEveryoneAnswers(input)")
    val result = sumEveryoneAnswers(specInputs)

    Then(s"Result is 6")
    result shouldBe 6
  }

  "part2 - sumEveryoneAnswers" should "return solution" in {
    Given("Puzzle input")

    When(s"sumEveryoneAnswers(input)")
    val result = sumEveryoneAnswers(puzzleInputs)

    Then(s"Result is 3232")
    result shouldBe 3232
  }

}
