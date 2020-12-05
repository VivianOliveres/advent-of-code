package com.kensai.aoc

import com.kensai.aoc.Day05._
import org.junit.runner.RunWith
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day05Suite extends AnyFlatSpec {

  private val InputPath = "src/test/resources/Day05.input"

  private val Row1: String = "FBFBBFFRLR"
  private val Row2: String = "BFFFBBFRRR"
  private val Row3: String = "FFFBBBFRRR"
  private val Row4: String = "BBFFBBFRLL"

  s"computeRow for $Row1" should "return 44" in {
    // GIVEN: input from specs
    val input = Row1

    // WHEN: computeRow
    val result = computeRow(input)

    // THEN: 44
    result should be(44)
  }

  s"computeColumn for $Row1" should "return 5" in {
    // GIVEN: input from specs
    val input = Row1

    // WHEN: computeColumn
    val result = computeColumn(input)

    // THEN: 5
    result should be(5)
  }

  s"computeSeatId for $Row1" should "return 357" in {
    // GIVEN: input from specs
    val input = Row1

    // WHEN: computeSeatId
    val result = computeSeatId(input)

    // THEN: 357
    result should be(357)
  }

  s"computeRow for $Row2" should "return 70" in {
    // GIVEN: input from specs
    val input = Row2

    // WHEN: computeRow
    val result = computeRow(input)

    // THEN: 70
    result should be(70)
  }

  s"computeColumn for $Row2" should "return 7" in {
    // GIVEN: input from specs
    val input = Row2

    // WHEN: computeColumn
    val result = computeColumn(input)

    // THEN: 7
    result should be(7)
  }

  s"computeSeatId for $Row2" should "return 567" in {
    // GIVEN: input from specs
    val input = Row2

    // WHEN: computeSeatId
    val result = computeSeatId(input)

    // THEN: 567
    result should be(567)
  }

  s"computeRow for $Row3" should "return 14" in {
    // GIVEN: input from specs
    val input = Row3

    // WHEN: computeRow
    val result = computeRow(input)

    // THEN: 14
    result should be(14)
  }

  s"computeColumn for $Row3" should "return 7" in {
    // GIVEN: input from specs
    val input = Row3

    // WHEN: computeRow
    val result = computeColumn(input)

    // THEN: 7
    result should be(7)
  }

  s"computeSeatId for $Row3" should "return 119" in {
    // GIVEN: input from specs
    val input = Row3

    // WHEN: computeRow
    val result = computeSeatId(input)

    // THEN: 119
    result should be(119)
  }

  s"computeRow for $Row4" should "return 102" in {
    // GIVEN: input from specs
    val input = Row4

    // WHEN: computeRow
    val result = computeRow(input)

    // THEN: 102
    result should be(102)
  }

  s"computeColumn for $Row4" should "return 4" in {
    // GIVEN: input from specs
    val input = Row4

    // WHEN: computeColumn
    val result = computeColumn(input)

    // THEN: 4
    result should be(4)
  }

  s"computeSeatId for $Row4" should "return 820" in {
    // GIVEN: input from specs
    val input = Row4

    // WHEN: computeSeatId
    val result = computeSeatId(input)

    // THEN: 820
    result should be(820)
  }

  "computeHighestSeatId for (Row1, Row2, Row3, Row4))" should "return 820" in {
    // GIVEN: input from specs
    val inputs = List(Row1, Row2, Row3, Row4)

    // WHEN: computeHighestSeatId
    val result = computeHighestSeatId(inputs)

    // THEN: 820
    result should be(820)
  }

  "computeHighestSeatId" should "find solution" in {
    // GIVEN: input from specs
    val inputs = readInputFile(InputPath)

    // WHEN: computeHighestSeatId
    val result = computeHighestSeatId(inputs)

    // THEN: 818
    result should be(818)
  }

  "findSeatId" should "find solution" in {
    // GIVEN: input from specs
    val inputs = readInputFile(InputPath)

    // WHEN: findSeatId
    val result = findSeatId(inputs)

    // THEN: ID is 559
    result should be(559)
  }

  private def readInputFile(path: String): List[String] =
    Source.fromFile(path)
      .getLines
      .toList

}
