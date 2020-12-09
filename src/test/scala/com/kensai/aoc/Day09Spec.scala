package com.kensai.aoc

import com.kensai.aoc.Day09._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day09Spec extends AnyFlatSpec with GivenWhenThen {

  private val InputPath = "src/test/resources/Day09.input"
  private val InputSpecPath = "src/test/resources/Day09Spec.input"


  "compute for spec" should "return 127" in {
    Given(s"Input is spec")
    val inputs = readInputFile(InputSpecPath)

    When(s"compute(input)")
    val result = compute(5, inputs)

    Then(s"Result is 127")
    result shouldBe 127L
  }

  "compute for puzzle" should "return solution" in {
    Given(s"Puzzle input")
    val inputs = readInputFile(InputPath)

    When(s"compute(input)")
    val result = compute(25, inputs)

    Then(s"Result is 507622668")
    result shouldBe 507622668L
  }

  "compute2 for spec" should "return 127" in {
    Given(s"Input is spec")
    val inputs = readInputFile(InputSpecPath)

    When(s"compute(input)")
    val result = compute2(5, inputs)

    Then(s"Result is 127")
    result shouldBe 62L
  }

  "compute2 for spec" should "return XXX" in {
    Given(s"Input is spec")
    val inputs = readInputFile(InputPath)

    When(s"compute(input)")
    val result = compute2(25, inputs)

    Then(s"Result is 76688505")
    result shouldBe 76688505L
  }

  //*********************************************************

//  "compute2 for spec" should "return 8" in {
//    Given(s"Input is spec")
//    val inputs = readInputFile(InputSpecPath)
//
//    When(s"accumulatorAfterFixingInputs(input)")
//    val result = compute2(inputs)
//
//    Then(s"Result is 8")
//    result shouldBe 8
//  }
//
//  "compute2 for puzzle" should "return solution" in {
//    Given(s"Puzzle solution")
//    val inputs = readInputFile(InputPath)
//
//    When(s"accumulatorAfterFixingInputs(input)")
//    val result = compute2(inputs)
//
//    Then(s"Result is 1984")
//    result shouldBe 1984
//  }

  private def readInputFile(path: String): List[String] =
    Source.fromFile(path)
      .getLines
      .toList

}
