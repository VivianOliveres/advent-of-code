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
