package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day10._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day10Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day10.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day10Spec.input"
  )

  "computeCorruptedSyntaxScore({([(<{}[<>[]}>{[]{[(<()>)" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "{([(<{}[<>[]}>{[]{[(<()>"

    When("computeCorruptedSyntaxScore(input)")
    val result = computeCorruptedSyntaxScore(Seq(i))

    Then("Result is 1197")
    result shouldBe 1197
  }

  "computeCorruptedSyntaxScore([[<[([]))<([[{}[[()]]])" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "[[<[([]))<([[{}[[()]]]"

    When("computeCorruptedSyntaxScore(input)")
    val result = computeCorruptedSyntaxScore(Seq(i))

    Then("Result is 3")
    result shouldBe 3
  }

  "computeCorruptedSyntaxScore([{[{({}]{}}([{[{{{}}([])" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "[{[{({}]{}}([{[{{{}}([]"

    When("computeCorruptedSyntaxScore(input)")
    val result = computeCorruptedSyntaxScore(Seq(i))

    Then("Result is 57")
    result shouldBe 57
  }

  "computeCorruptedSyntaxScore([<(<(<(<{}))><([]([]())" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "[<(<(<(<{}))><([]([]()"

    When("computeCorruptedSyntaxScore(input)")
    val result = computeCorruptedSyntaxScore(Seq(i))

    Then("Result is 3")
    result shouldBe 3
  }

  "computeCorruptedSyntaxScore(<{([([[(<>()){}]>(<<{{" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "<{([([[(<>()){}]>(<<{{"

    When("computeCorruptedSyntaxScore(input)")
    val result = computeCorruptedSyntaxScore(Seq(i))

    Then("Result is 25137")
    result shouldBe 25137
  }

  "computeCorruptedSyntaxScore" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeCorruptedSyntaxScore(input)")
    val result = computeCorruptedSyntaxScore(puzzleSpecInput)

    Then("Result is 26397")
    result shouldBe 26397
  }

  "computeCorruptedSyntaxScore" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeCorruptedSyntaxScore(puzzleInput)")
    val result = computeCorruptedSyntaxScore(puzzleInput)

    Then("Result is expected")
    result shouldBe 411471
  }

  "computeIncompleteSyntaxScore [({(<(())[]>[[{[]{<()<>>" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "[({(<(())[]>[[{[]{<()<>>"

    When("computeIncompleteSyntaxScore(input)")
    val result = computeIncompleteSyntaxScore(Seq(i))

    Then("Result is 288957")
    result shouldBe Seq(288957)
  }

  "computeIncompleteSyntaxScore [(()[<>])]({[<{<<[]>>(" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "[(()[<>])]({[<{<<[]>>("

    When("computeIncompleteSyntaxScore(input)")
    val result = computeIncompleteSyntaxScore(Seq(i))

    Then("Result is 5566")
    result shouldBe Seq(5566)
  }

  "computeIncompleteSyntaxScore (((({<>}<{<{<>}{[]{[]{}" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "(((({<>}<{<{<>}{[]{[]{}"

    When("computeIncompleteSyntaxScore(input)")
    val result = computeIncompleteSyntaxScore(Seq(i))

    Then("Result is 1480781")
    result shouldBe Seq(1480781)
  }

  "computeIncompleteSyntaxScore {<[[]]>}<{[{[{[]{()[[[]" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "{<[[]]>}<{[{[{[]{()[[[]"

    When("computeIncompleteSyntaxScore(input)")
    val result = computeIncompleteSyntaxScore(Seq(i))

    Then("Result is 995444")
    result shouldBe Seq(995444)
  }

  "computeIncompleteSyntaxScore <{([{{}}[<[[[<>{}]]]>[]]" should "find result from spec input" in {
    Given("Puzzle spec input")
    val i = "<{([{{}}[<[[[<>{}]]]>[]]"

    When("computeIncompleteSyntaxScore(input)")
    val result = computeIncompleteSyntaxScore(Seq(i))

    Then("Result is 294")
    result shouldBe Seq(294)
  }

  "computeMiddleIncompleteSyntaxScore" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("computeMiddleIncompleteSyntaxScore(input)")
    val result = computeMiddleIncompleteSyntaxScore(puzzleSpecInput)

    Then("Result is 288957")
    result shouldBe 288957
  }

  "computeMiddleIncompleteSyntaxScore" should "find result from input" in {
    // GIVEN: input
    Given("Puzzle input")

    When("computeMiddleIncompleteSyntaxScore(puzzleInput)")
    val result = computeMiddleIncompleteSyntaxScore(puzzleInput)

    Then("Result is expected")
    result shouldBe 3122628974L
  }
}
