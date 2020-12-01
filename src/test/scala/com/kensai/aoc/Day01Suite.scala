package com.kensai.aoc

import com.kensai.aoc.Day01.compute
import org.scalatest._
import matchers.should.Matchers._
import flatspec._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day01Suite extends AnyFlatSpec {

  val ExpectedSum = 2020

  "compute" should "find result from first example" in {
    // GIVEN: input from specs
    val input = List(1721L, 979L, 366L, 299L, 675L, 1456L)

    // WHEN: compute
    val result = compute(ExpectedSum, input)

    // THEN: result is 514579
    val expectedResult = Some(514579)
    result should be (expectedResult)
  }

  "compute" should "return empty if there is no result" in {
    // GIVEN: input from specs
    val input = List(3L, 5L)

    // WHEN: compute
    val result = compute(ExpectedSum, input)

    // THEN: result is None
    result should be (None)
  }

  "compute" should "return empty for an empty list" in {
    // GIVEN: input from specs
    val input: List[Long] = List()

    // WHEN: compute
    val result = compute(ExpectedSum, input)

    // THEN: result is None
    result should be (None)
  }

  "compute" should "find result from input" in {
    // GIVEN: input from specs
    val input = readInputFile("src/test/resources/Day01.input")

    // WHEN: compute
    val result = compute(ExpectedSum, input)

    // THEN: result is defined
    println(result)
    result shouldBe defined
  }

  private def readInputFile(path: String): List[Long] =
    Source.fromFile(path)
      .getLines
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toLong)
      .toList
}
