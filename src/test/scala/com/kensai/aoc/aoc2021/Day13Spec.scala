package com.kensai.aoc.aoc2021

import Day13._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day13Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputLines(
    "src/test/resources/2021//Day13.input"
  )
  private lazy val specInputs = readInputLines(
    "src/test/resources/2021//Day13Spec.input"
  )

  "parse from specs" should "return (939L, Set(7,13,59,31,19))" in {
    Given(s"Input is \n$specInputs")

    When("parse")
    val result = parse(specInputs)

    Then(s"Result is 1L")
    result shouldBe (939L, Set(7L, 13L, 59L, 31L, 19L))
  }

  "computeEarliestBus from specs" should "return Some((944L, 59L))" in {
    Given(s"Input is \n$specInputs")
    val i = parse(specInputs)

    When("computeEarliestBus")
    val result = computeEarliestBus(i)

    Then(s"Result is 1L")
    result shouldBe Some((944L, 59L))
  }

  "computePart1 from specs" should "return 295L" in {
    Given(s"Input is \n$specInputs")

    When("computePart1")
    val result = computePart1(specInputs)

    Then(s"Result is 1L")
    result shouldBe 295L
  }

  "computePart1 from puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When("parse")
    val result = computePart1(puzzleInputs)

    Then(s"Result is 2545L")
    result shouldBe 2545L
  }

  "parseRow2 from specs" should "return Map(7 -> 0, 13 -> 1, 59 -> 4, 31 -> 6, 19 -> 7)" in {
    Given(s"Input is \n$specInputs")

    When("parse2")
    val result = parseRow2(specInputs)

    Then(s"Result is 1L")
    result shouldBe Map(7L -> 0L, 13L -> 1L, 59L -> 4L, 31L -> 6L, 19L -> 7L)
  }

  "compute2 from specs" should "return 1068781L" in {
    Given(s"Input is \n$specInputs")
    val input = parseRow2(specInputs)

    When("compute2")
    val result = compute2(input)

    Then(s"Result is 1068781L")
    result shouldBe 1068781L
  }

  "ei" should "return valid" in {
    val n = 3L * 5L * 7L
    ei(3, n) shouldBe 70L
    ei(5, n) shouldBe 21L
    ei(7, n) shouldBe 15L

    val result = 2 * ei(3, n) + 3 * ei(5, n) + 2 * ei(7, n)
    result shouldBe 233L
  }

  "chineseRemainderTheorem for Map(3 -> 2, 5 -> 3, 7 -> 2)" should "return 23" in {
    val input = Map(3L -> 2L, 5L -> 3L, 7L -> 2L)
    Given(s"Input is $input")

    When("chineseRemainderTheorem")
    val result = chineseRemainderTheorem(input)

    Then(s"Result is 23")
    result shouldBe 23L
  }

  "chineseRemainderTheorem for Map(3 -> 1, 4 -> 2, 5 -> 3)" should "return 58" in {
    val input = Map(3L -> 1L, 4L -> 2L, 5L -> 3L)
    Given(s"Input is $input")

    When("chineseRemainderTheorem")
    val result = chineseRemainderTheorem(input)

    Then(s"Result is 58")
    result shouldBe 58L
  }

  "compute2 from specs0" should "return 1068781" in {
    val input = Map(7L -> 0L, 13L -> 1L, 59L -> 4L, 31L -> 6L, 19L -> 7L)
    Given(s"Input is $input")

    When("chineseRest")
    val result = compute2(input)

    Then(s"Result is 1068781")
    result shouldBe 1068781L
  }

  "compute2 from specs1" should "return 3417" in {
    val input = Map(17L -> 0L, 13L -> 2L, 19L -> 3L)
    Given(s"Input is $input")

    When("compute2")
    val result = compute2(input)

    Then(s"Result is 3417L")
    result shouldBe 3417L
  }

  "compute2 from specs5" should "return 1_202_161_486" in {
    val input = Map(1789L -> 0L, 37L -> 1L, 47L -> 2L, 1889L -> 3L)
    Given(s"Input is $input")

    When("compute2")
    val result = compute2(input)

    Then(s"Result is 1202161486L")
    result shouldBe 1202161486L
  }

  "chineseRest from puzzle" should "return 266204454441577" in {
    val input = parseRow2(puzzleInputs)
    Given(s"Input is \n$input")

    When("compute2")
    val result = compute2(input)

    Then(s"Result is 266204454441577")
    result shouldBe 266204454441577L
  }

}
