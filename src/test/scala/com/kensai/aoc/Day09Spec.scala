package com.kensai.aoc

import com.kensai.aoc.Day09._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day09Spec extends AnyFlatSpec with GivenWhenThen {

  private val InputPath = "src/test/resources/Day09.input"
  private val InputSpecPath = "src/test/resources/Day09Spec.input"

  "firstInvalidNumber for spec" should "return 127" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpecPath)

    When(s"firstInvalidNumber(input)")
    val result = firstInvalidNumber(5, inputs)

    Then(s"Result is 127")
    result shouldBe 127L
  }

  "firstInvalidNumber for puzzle" should "return solution" in {
    Given(s"Puzzle input")
    val inputs = readInputLines(InputPath)

    When(s"firstInvalidNumber(input)")
    val result = firstInvalidNumber(25, inputs)

    Then(s"Result is 507622668")
    result shouldBe 507622668L
  }

  "findContiguousSet for spec" should "return 62" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpecPath)

    When(s"findContiguousSet(input)")
    val result = findContiguousSet(5, inputs)

    Then(s"Result is 62")
    result shouldBe 62L
  }

  "findContiguousSet for spec" should "return solution" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputPath)

    When(s"findContiguousSet(input)")
    val result = findContiguousSet(25, inputs)

    Then(s"Result is 76688505")
    result shouldBe 76688505L
  }

}
