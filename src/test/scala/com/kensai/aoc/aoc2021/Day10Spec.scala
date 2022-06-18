package com.kensai.aoc.aoc2021

import Day10._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day10Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputLines(
    "src/test/resources/2021//Day10.input"
  )
  private lazy val specInputs = readInputLines(
    "src/test/resources/2021//Day10Spec.input"
  )

  private val smallExample = """
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

  "computeDiff for small example" should "return (7, 0, 5)" in {
    Given(s"Input is small example")

    When(s"computeDiff(input)")
    val result = computeDiff(smallExample)

    Then(s"Result is (7, 0, 5)")
    result shouldBe (7, 0, 5)
  }

  "computeResult for small example" should "return 35" in {
    Given(s"Input is small example")

    When(s"computeResult(input)")
    val result = computeResult(smallExample)

    Then(s"Result is 35")
    result shouldBe 7 * 5
  }

  "computeDiff for spec" should "return (22, 0, 10)" in {
    Given(s"Input is spec")

    When(s"computeDiff(input)")
    val result = computeDiff(specInputs)

    Then(s"Result is (22, 0, 10)")
    result shouldBe (22, 0, 10)
  }

  "computeResult for spec" should "return 220" in {
    Given(s"Input is spec")

    When(s"computeResult(input)")
    val result = computeResult(specInputs)

    Then(s"Result is 220")
    result shouldBe 22 * 10
  }

  "computeResult for puzzle" should "return solution" in {
    Given(s"Puzzle input")

    When(s"computeResult(input)")
    val result = computeResult(puzzleInputs)

    Then(s"Result is 2232")
    result shouldBe 2232L
  }

  "pathReachingMaxCount for small example" should "return 8" in {
    Given(s"Input is small example")

    When(s"pathReachingMaxCount(input)")
    val result = pathReachingMaxCount(smallExample)

    Then(s"Result is 8")
    result shouldBe 8L
  }

  "compute2 for spec" should "return 19208" in {
    Given(s"Input is spec")

    When(s"compute(input)")
    val result = pathReachingMaxCount(specInputs)

    Then(s"Result is 19208")
    result shouldBe 19208L
  }

  "compute2 for solution" should "find solution" in {
    Given(s"Input is spec")

    When(s"compute(input)")
    val result = pathReachingMaxCount(puzzleInputs)

    Then(s"Result is 173625106649344")
    result shouldBe 173625106649344L
  }

}
