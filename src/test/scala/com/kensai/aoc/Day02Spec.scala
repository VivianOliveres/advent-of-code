package com.kensai.aoc

import com.kensai.aoc.Day02._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day02Spec extends AnyFlatSpec with GivenWhenThen with Day02Fixtures {

  "validPasswordCount for rows from specs" should "return 2 for Part1" in {
    Given(s"Input is List(Row1, Row2, Row3)")
    val inputs = List(Row1, Row2, Row3)

    When("validPasswordCount(inputs)(isPasswordValidPart1)")
    val result = validPasswordCount(inputs)(isPasswordValidPart1)

    Then(s"Result is 2")
    result shouldBe 2
  }

  "validPasswordCount" should "return solution for Part1" in {
    Given(s"Puzzle input")
    val inputs = readInputFile("src/test/resources/Day02.input")

    When("validPasswordCount(inputs)(isPasswordValidPart1)")
    val result = validPasswordCount(inputs)(isPasswordValidPart1)

    Then(s"Solution is 556")
    result shouldBe 556
  }

  "validPasswordCount for rows from specs" should "return 1 for Part2" in {
    Given(s"Input is List(Row1, Row2, Row3)")
    val inputs = List(Row1, Row2, Row3)

    When("validPasswordCount(inputs)(isPasswordValidPart2)")
    val result = validPasswordCount(inputs)(isPasswordValidPart2)

    Then(s"Result is 1")
    result shouldBe 1
  }

  "validPasswordCount" should "return solution for Part2" in {
    Given(s"Puzzle input")
    val inputs = readInputFile("src/test/resources/Day02.input")

    When("validPasswordCount(inputs)(isPasswordValidPart2)")
    val result = validPasswordCount(inputs)(isPasswordValidPart2)

    Then(s"Result is 605")
    result shouldBe 605
  }

  private def readInputFile(path: String): List[PasswordRow] =
    readInputLines(path)
      .map(_.trim)
      .filterNot(_.isEmpty)
      .flatMap(parse)
      .toList

}
