package com.kensai.aoc.aoc2021

import Day18._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day18Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputLines(
    "src/test/resources/2021//Day18.input"
  )
//  private lazy val specInputs = readInputLines("src/test/resources/2021//Day18Spec.input")

  "parse" should "return " in {

    parse("1 + 2") shouldBe 3
    parse("1 + 2 + 3") shouldBe 6
    parse("1 * 2") shouldBe 2
    parse("1 * 2 + 3") shouldBe 5
    parse("1 * 2 + 3 * 4") shouldBe 20

    parse("(1 + 2)") shouldBe 3
    parse("(1 + 2 + 3)") shouldBe 6
    parse("1 + (2 + 3)") shouldBe 6
    parse("(1 + 2) + 3") shouldBe 6

    parse("1 + (2 * 3) + (4 * (5 + 6))") shouldBe 51
  }

  "compute for puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When(s"compute(inputs)")
    val result = compute(puzzleInputs)

    Then(s"Result is 510009915468")
    result shouldBe 510009915468L
  }

  "compute for part2" should "return " in {
    Given(s"Input is puzzle")

    compute2(List("1 + 2 * 3 + 4 * 5 + 6")) shouldBe 231L
    compute2(List("1 + (2 * 3) + (4 * (5 + 6))")) shouldBe 51L
  }

  "compute2 for puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When(s"compute(inputs)")
    val result = compute2(puzzleInputs)

    Then(s"Result is 321176691637769")
    result shouldBe 321176691637769L
  }

}
