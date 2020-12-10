package com.kensai.aoc

import com.kensai.aoc.Day07._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day07Spec extends AnyFlatSpec with GivenWhenThen {

  private val InputPath = "src/test/resources/Day07.input"
  private val InputSpec1Path = "src/test/resources/Day07Spec1.input"
  private val InputSpec2Path = "src/test/resources/Day07Spec2.input"

  "parseColorContainedBy for spec" should "return a map of 7" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpec1Path)

    When(s"parseColorContainedBy(input)")
    val result = parseColorContainedBy(inputs)

    Then(s"Result is a map of 7")
    result("bright white") shouldBe Set("light red", "dark orange")
    result("muted yellow") shouldBe Set("light red", "dark orange")
    result("shiny gold") shouldBe Set("bright white", "muted yellow")
    result("faded blue") shouldBe Set("muted yellow", "dark olive", "vibrant plum")
    result("dark olive") shouldBe Set("shiny gold")
    result("vibrant plum") shouldBe Set("shiny gold")
    result("dotted black") shouldBe Set("dark olive", "vibrant plum")
    result should have size 7
  }

  "bagsContaining(shiny gold) for specs" should "return 4" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpec1Path)

    When(s"bagsContaining(shiny gold, input)")
    val result = bagsContaining("shiny gold", inputs)

    Then(s"Result is 4")
    result shouldBe 4
  }

  "bagsContaining(shiny gold) for inputs" should "find solution" in {
    Given(s"Puzzle input")
    val inputs = readInputLines(InputPath)

    When(s"bagsContaining(shiny gold, input)")
    val result = bagsContaining("shiny gold", inputs)

    Then(s"Result is 142")
    result shouldBe 142L
  }

  "parseColorContaining for spec" should "return a map of 9" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpec1Path)

    When(s"parseColorContaining()")
    val result = parseColorContaining(inputs)

    Then(s"Result is empty")
    result should have size 9
    result("light red") shouldBe Map("bright white" -> 1, "muted yellow" -> 2)
    result("dark orange") shouldBe Map("bright white" -> 3, "muted yellow" -> 4)
    result("bright white") shouldBe Map("shiny gold" -> 1)
    result("muted yellow") shouldBe Map("shiny gold" -> 2, "faded blue" -> 9)
    result("shiny gold") shouldBe Map("dark olive" -> 1, "vibrant plum" -> 2)
    result("dark olive") shouldBe Map("faded blue" -> 3, "dotted black" -> 4)
    result("vibrant plum") shouldBe Map("faded blue" -> 5, "dotted black" -> 6)
    result("faded blue") shouldBe Map()
    result("dotted black") shouldBe Map()
  }

  "computeBagContainingOtherBags(shiny gold) for specs" should "return 32" in {
    Given(s"Input is spec")
    val inputs = readInputLines(InputSpec1Path)

    When(s"computeBagContainingOtherBags(shiny gold, input)")
    val result = computeBagContainingOtherBags("shiny gold", inputs)

    Then(s"Result is 32")
    result shouldBe 32
  }

  "computeBagContainingOtherBAgs(shiny gold) for specs2" should "return 126" in {
    Given(s"Input is spec2")
    val inputs = readInputLines(InputSpec2Path)

    When(s"computeBagContainingOtherBags(shiny gold, input)")
    val result = computeBagContainingOtherBags("shiny gold", inputs)

    Then(s"Result is 126")
    result shouldBe 126
  }

  "compute2 (shiny gold) for inputs" should "find solution" in {
    Given(s"Puzzle input")
    val inputs = readInputLines(InputPath)

    When(s"computeBagContainingOtherBags(shiny gold, input)")
    val result = computeBagContainingOtherBags("shiny gold", inputs)

    Then(s"Result is 10219")
    result shouldBe 10219L
  }

}
