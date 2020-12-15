package com.kensai.aoc

import com.kensai.aoc.Day15._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15Spec extends AnyFlatSpec with GivenWhenThen {

  "parse for puzzle input" should "return valid result" in {
    val input = "0,1,4,13,15,12,16"
    Given(s"Input is $input")

    When("parse")
    val result = parse(input)

    Then(s"Result is List(0, 1, 4, 13, 15, 12, 16)")
    result shouldBe List(0, 1, 4, 13, 15, 12, 16)
  }

  "lastSpokenAt(2020) for puzzle" should "return solution" in {
    val input = parse("0,1,4,13,15,12,16")
    Given(s"Input is $input")

    When("lastSpokenAt(input, 2020)")
    val result = lastSpokenAt(input, 2020)

    Then(s"Result is 1665")
    result shouldBe 1665
  }

  "lastSpokenAt(30000000) for puzzle" should "return solution" in {
    val input = parse("0,1,4,13,15,12,16")
    Given(s"Input is $input")

    When("lastSpokenAt(input, 30000000)")
    val result = lastSpokenAt(input, 30000000)

    Then(s"Result is 16439")
    result shouldBe 16439
  }

}
