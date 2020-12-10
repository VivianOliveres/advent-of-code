package com.kensai.aoc

import com.kensai.aoc.Day10._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day10Spec extends AnyFlatSpec with GivenWhenThen {

  private val InputPath = "src/test/resources/Day10.input"
  private val InputSpecPath = "src/test/resources/Day10Spec.input"

  "computeDiff for ex" should "return XXX" in {
    Given(s"Input is spec")
    val inputs =
      """
        |16
        |10
        |15
        |5
        |1
        |11
        |7
        |19
        |6
        |12
        |4
        |""".stripMargin.split("\n").toList

    When(s"computeDiff(input)")
    val result = computeDiff(inputs)

    Then(s"Result is (7, 0, 5)")
    result shouldBe (7, 0, 5)
  }

  "computeResult for ex" should "return XXX" in {
    Given(s"Input is spec")
    val inputs =
      """
        |16
        |10
        |15
        |5
        |1
        |11
        |7
        |19
        |6
        |12
        |4
        |""".stripMargin.split("\n").toList

    When(s"computeResult(input)")
    val result = computeResult(inputs)

    Then(s"Result is 35")
    result shouldBe 7 * 5
  }

  "computeDiff for spec" should "return (22, 0, 10)" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpecPath)

    When(s"computeDiff(input)")
    val result = computeDiff(inputs)

    Then(s"Result is (22, 0, 10)")
    result shouldBe (22, 0, 10)
  }

  "computeResult for spec" should "return 220" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpecPath)

    When(s"computeResult(input)")
    val result = computeResult(inputs)

    Then(s"Result is 220")
    result shouldBe 22 * 10
  }

  "computeResult for puzzle" should "return solution" in {
    Given(s"Puzzle input")
    val inputs = readInputLines(InputPath)

    When(s"computeResult(input)")
    val result = computeResult(inputs)

    Then(s"Result is 2232")
    result shouldBe 2232L
  }

  "pathReachingMaxCount for small example" should "return 8" in {
    Given(s"Input is small example")
    val inputs =
      """
        |16
        |10
        |15
        |5
        |1
        |11
        |7
        |19
        |6
        |12
        |4
        |""".stripMargin.split("\n").toList

    When(s"pathReachingMaxCount(input)")
    val result = pathReachingMaxCount(inputs)

    Then(s"Result is 8")
    result shouldBe 8L
  }

  "compute2 for spec" should "return 19208" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpecPath)

    When(s"compute(input)")
    val result = pathReachingMaxCount(inputs)

    Then(s"Result is 19208")
    result shouldBe 19208L
  }

  "compute2 for solution" should "find solution" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputPath)

    When(s"compute(input)")
    val result = pathReachingMaxCount(inputs)

    Then(s"Result is 173625106649344")
    result shouldBe 173625106649344L
  }

}
