package com.kensai.aoc

import com.kensai.aoc.Day06._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day06Spec extends AnyFlatSpec with GivenWhenThen {

  private val InputPath = "src/test/resources/Day06.input"
  private val InputSpecPath = "src/test/resources/Day06Spec.input"

  "part1: sumDifAnswers from specs" should "return 11" in {
    Given("Spec input")
    val input = readInputFile(InputSpecPath)

    When(s"sumDifAnswers(input)")
    val result = sumDifAnswers(input)

    Then(s"Result is 11")
    result shouldBe 11
  }

  "part1: sumDifAnswers" should "return solution" in {
    Given("Puzzle input")
    val input = readInputFile(InputPath)

    When(s"sumDifAnswers(input)")
    val result = sumDifAnswers(input)

    Then(s"Result is 6443")
    result shouldBe 6443
  }


  "part2: sumEveryoneAnswers from spec" should "return 6" in {
    Given("Spec input")
    val input = readInputFile(InputSpecPath)

    When(s"sumEveryoneAnswers(input)")
    val result = sumEveryoneAnswers(input)

    Then(s"Result is 6")
    result shouldBe 6
  }

  "part2 - sumEveryoneAnswers" should "return solution" in {
    Given("Puzzle input")
    val input = readInputFile(InputPath)

    When(s"sumEveryoneAnswers(input)")
    val result = sumEveryoneAnswers(input)

    Then(s"Result is 3232")
    result shouldBe 3232
  }

}
