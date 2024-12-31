package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day23._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day23Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2024/Day23.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2024/Day23Spec.input"
  )

  "extract(puzzleSpecInput, t)" should "work for puzzle spec" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("extract(input, t)")
    val result = extract(input, "t")

    Then("Result is 7 elements")
    result should have size 7
  }

  "extract(puzzleInput, t)" should "work for puzzle" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("extract(input, t, 3)")
    val result = extract(input, "t")

    Then("Result is expected")
    result should have size 1054
  }

  "computePassword(puzzleSpecInput)" should "find solution for puzzle spec" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("computePassword(input)")
    val result = computePassword(input)

    Then("Result is co,de,ka,ta")
    result shouldBe "co,de,ka,ta"
  }

  "computePassword(puzzleSpecInput)" should "find solution for puzzle" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("computePassword(input)")
    val result = computePassword(input)

    Then("Result is expected")
    result shouldBe "ch,cz,di,gb,ht,ku,lu,tw,vf,vt,wo,xz,zk"
  }

}
