package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day22._
import com.kensai.aoc.lib.Lib.readLongInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day22Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readLongInputLines(
    "src/test/resources/2024/Day22.input"
  )

  private lazy val puzzleSpecInput = readLongInputLines(
    "src/test/resources/2024/Day22Spec.input"
  )

  "computeNextSecretNumber(123)" should "return result" in {
    val r1 = computeNextSecretNumber(123L)
    r1 shouldBe 15887950L

    val r2 = computeNextSecretNumber(15887950L)
    r2 shouldBe 16495136L

    val r4 = computeNextSecretNumber(527345L)
    r4 shouldBe 704524L

    val r5 = computeNextSecretNumber(704524L)
    r5 shouldBe 1553684L

    val r6 = computeNextSecretNumber(1553684L)
    r6 shouldBe 12683156L

    val r7 = computeNextSecretNumber(12683156L)
    r7 shouldBe 11100544L

    val r8 = computeNextSecretNumber(11100544L)
    r8 shouldBe 12249484L

    val r9 = computeNextSecretNumber(12249484L)
    r9 shouldBe 7753432L

    val r10 = computeNextSecretNumber(7753432L)
    r10 shouldBe 5908254L
  }

  "computeSecretNumber(1, 2000)" should "return 8685429" in {
    When("computeSecretNumber(1, 2000)")
    val result = computeSecretNumber(1L, 2000)

    Then("Result is 8685429")
    result shouldBe 8685429L
  }

  "computeSecretNumber(10, 2000)" should "return 4700978" in {
    When("computeSecretNumber(10, 2000)")
    val result = computeSecretNumber(10L, 2000)

    Then("Result is 4700978")
    result shouldBe 4700978L
  }

  "computeSecretNumber(100, 2000)" should "return 15273692" in {
    When("computeSecretNumber(100, 2000)")
    val result = computeSecretNumber(100L, 2000)

    Then("Result is 15273692")
    result shouldBe 15273692L
  }

  "computeSecretNumber(2024, 2000)" should "return 8667524" in {
    When("computeSecretNumber(2024, 2000)")
    val result = computeSecretNumber(2024L, 2000)

    Then("Result is 8667524")
    result shouldBe 8667524L
  }

  "sumBuyersSecretNumbers(input, 2000)" should "find 37327623 from puzzle Spec input" in {
    Given("Puzzle input")
    val input = puzzleSpecInput

    When("sumBuyersSecretNumbers(input, 2000)")
    val result = sumBuyersSecretNumbers(input, 2000)

    Then("Result is 37327623")
    result shouldBe 37327623L
  }

  "sumBuyersSecretNumbers(input, 2000)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = puzzleInput

    When("sumBuyersSecretNumbers(input, 2000)")
    val result = sumBuyersSecretNumbers(input, 2000)

    Then("Result is expected")
    result shouldBe 20071921341L
  }

  "sumBananasSequences(input, 2000)" should "find 23 from (1,2,3,2024)" in {
    When("sumBananasSequences(input, 2000)")
    val result = sumBananasSequences(Seq(1,2,3,2024), 2000)

    Then("Result is 23")
    result shouldBe 23
  }

  "sumBananasSequences(input, 2000)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = puzzleInput

    When("sumBananasSequences(input, 2000)")
    val result = sumBananasSequences(input, 2000)

    Then("Result is expected")
    result shouldBe 2242
  }

}
