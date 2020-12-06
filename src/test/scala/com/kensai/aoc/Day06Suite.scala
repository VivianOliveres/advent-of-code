package com.kensai.aoc

import com.kensai.aoc.Day06._
import org.junit.runner.RunWith
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day06Suite extends AnyFlatSpec {

  private val InputPath = "src/test/resources/Day06.input"
  private val InputSpecPath = "src/test/resources/Day06Spec.input"

  "part1: sumDifAnswers from specs" should "return 11" in {
    // GIVEN: inputs from specs
    val input = readInputFile(InputSpecPath)

    // WHEN: sumDifAnswers
    val result = sumDifAnswers(input)

    // THEN: result is 11
    result should be(11)
  }

  "part1: sumDifAnswers" should "return solution" in {
    // GIVEN: input
    val input = readInputFile(InputPath)

    // WHEN: sumDifAnswers
    val result = sumDifAnswers(input)

    // THEN: result is 6443
    result should be(6443)
  }


  "part2: sumEveryoneAnswers from spec" should "return 6" in {
    // GIVEN: inputs from specs
    val input = readInputFile(InputSpecPath)

    // WHEN: sumEveryoneAnswers
    val result = sumEveryoneAnswers(input)

    // THEN: result is 6
    result should be(6)
  }

  "part2 - sumEveryoneAnswers" should "return solution" in {
    // GIVEN: input
    val input = readInputFile(InputPath)

    // WHEN: sumEveryoneAnswers
    val result = sumEveryoneAnswers(input)

    // THEN: result is 3232
    result should be(3232)
  }

  private def readInputFile(path: String): String =
    Source.fromFile(path)
      .getLines
      .toList
      .mkString("\n")

}
