package com.kensai.aoc

import com.kensai.aoc.Day05._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day05Spec extends AnyFlatSpec with GivenWhenThen with Day05Fixtures {

  private val InputPath = "src/test/resources/Day05.input"

  "computeHighestSeatId for (Row1, Row2, Row3, Row4))" should "return 820" in {
    val inputs = List(Row1, Row2, Row3, Row4)
    Given(s"Input is $inputs")

    When(s"computeHighestSeatId($inputs)")
    val result = computeHighestSeatId(inputs)

    Then(s"Result is 820")
    result should be(820)
  }

  "computeHighestSeatId" should "find solution" in {
    Given("Puzzle input")
    val inputs = readInputLines(InputPath)

    When(s"computeHighestSeatId(inputs)")
    val result = computeHighestSeatId(inputs)

    Then(s"Result is 818")
    result should be(818)
  }

  "findSeatId" should "find solution" in {
    Given("Puzzle input")
    val inputs = readInputLines(InputPath)

    When(s"findSeatId(inputs)")
    val result = findSeatId(inputs)

    Then(s"Result is 559")
    result should be(559)
  }

  "findSeatIdAlternative" should "find solution" in {
    Given("Puzzle input")
    val inputs = readInputLines(InputPath)

    When(s"findSeatIdAlternative(inputs)")
    val result = findSeatIdAlternative(inputs)

    Then(s"Result is 559")
    result should be(559)
  }

}
