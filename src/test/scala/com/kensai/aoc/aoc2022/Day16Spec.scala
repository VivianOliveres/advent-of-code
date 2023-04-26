package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day16._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day16Spec extends AnyFlatSpec with GivenWhenThen with Inside {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day16.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day16Spec.input"
  )

  "parse" should "return Sensors for spec input" in {
    // GIVEN: input
    Given("Puzzle Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val results = parse(input)

    Then("Result is expected")
    results("AA") shouldBe Valve("AA", 0, Seq("DD", "II", "BB"))
    results("BB") shouldBe Valve("BB", 13, Seq("CC", "AA"))
    results("CC") shouldBe Valve("CC", 2, Seq("DD", "BB"))
    results("DD") shouldBe Valve("DD", 20, Seq("CC", "AA", "EE"))
    results("EE") shouldBe Valve("EE", 3, Seq("FF", "DD"))
    results("FF") shouldBe Valve("FF", 0, Seq("EE", "GG"))
    results("GG") shouldBe Valve("GG", 0, Seq("FF", "HH"))
    results("HH") shouldBe Valve("HH", 22, Seq("GG"))
    results("II") shouldBe Valve("II", 0, Seq("AA", "JJ"))
    results("JJ") shouldBe Valve("JJ", 21, Seq("II"))
    results should have size 10
  }

  "findBestReleasePressure" should "return 1651 for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("findBestReleasePressure")
    val result = findBestReleasePressure(input)

    Then("Result is 1651")
    result shouldBe 1651
  }

  "findBestReleasePressure" should "return result for input" in {
    // GIVEN: input
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("findBestReleasePressure")
    val result = findBestReleasePressure(input)

    Then("Result is expected")
    result shouldBe 1584
  }

}
