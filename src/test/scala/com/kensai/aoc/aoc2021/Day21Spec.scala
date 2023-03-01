package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day21._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day21Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2021/Day21.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2021/Day21Spec.input"
  )

  "parse" should "find result from spec input" in {
    Given("Puzzle spec input")

    When("parse(puzzleSpecInput)")
    val result = parse(puzzleSpecInput)

    Then("Result is expected")
    result.player1 shouldBe Player(4, 0)
    result.player2 shouldBe Player(8, 0)
    result.rollCount shouldBe 0
  }

  "roll(50)" should "be 50" in {
    Given("")

    When("roll(50)")
    val result = roll(50, 100)

    Then("Result is 50")
    result shouldBe 50
  }

  "roll(101)" should "be 1" in {
    Given("")

    When("roll(101)")
    val result = roll(101, 100)

    Then("Result is 1")
    result shouldBe 1
  }

  "firstPlayerToPlay(0)" should "be true" in {
    Given("")

    When("firstPlayerToPlay(0)")
    val result = firstPlayerToPlay(0)

    Then("Result is true")
    result shouldBe true
  }

  "firstPlayerToPlay(3)" should "be false" in {
    Given("")

    When("firstPlayerToPlay(3)")
    val result = firstPlayerToPlay(3)

    Then("Result is false")
    result shouldBe false
  }

  "firstPlayerToPlay(6)" should "be true" in {
    Given("")

    When("firstPlayerToPlay(6)")
    val result = firstPlayerToPlay(6)

    Then("Result is true")
    result shouldBe true
  }

  "movePlayer(4, 1+2+3)" should "be 10" in {
    Given("")

    When("movePlayer(4, 6)")
    val result = movePlayer(4, 1 + 2 + 3)

    Then("Result is 10")
    result shouldBe 10
  }

  "movePlayer(8, 4+5+6)" should "be 3" in {
    Given("")

    When("movePlayer(8, 15)")
    val result = movePlayer(8, 4 + 5 + 6)

    Then("Result is 3")
    result shouldBe 3
  }

  "movePlayer(7, 2+2+1)" should "be 2" in {
    Given("")

    When("movePlayer(7, 5)")
    val result = movePlayer(7, 2 + 2 + 1)

    Then("Result is 2")
    result shouldBe 2
  }

  "movePlayer(6, 88+89+90)" should "be 3" in {
    Given("")

    When("movePlayer(6, 88+89+90)")
    val result = movePlayer(6, 88 + 89 + 90)

    Then("Result is 3")
    result shouldBe 3
  }

  "rollAndMove(firstInput)" should "as expected" in {
    Given("")
    val input = Day21Input(0, Player(4, 0), Player(8, 0))

    When("rollAndMove(input)")
    val result = rollAndMove(input, 100)

    Then("Result is expected")
    result shouldBe Day21Input(3, Player(10, 10), Player(8, 0))
  }

  "rollAndMove(second)" should "as expected" in {
    Given("")
    val input = Day21Input(3, Player(10, 10), Player(8, 0))

    When("rollAndMove(input)")
    val result = rollAndMove(input, 100)

    Then("Result is expected")
    result shouldBe Day21Input(6, Player(10, 10), Player(3, 3))
  }

  "rollAndMove(third)" should "as expected" in {
    Given("")
    val input = Day21Input(6, Player(10, 10), Player(3, 3))

    When("rollAndMove(input)")
    val result = rollAndMove(input, 100)

    Then("Result is expected")
    result shouldBe Day21Input(9, Player(4, 14), Player(3, 3))
  }

  "findSolution" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("findSolution(puzzleSpecInput)")
    val result = findSolution(input, 1000, 100)

    Then("Result is 739785")
    result shouldBe 739785L
  }

  "findSolution" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("findSolution(puzzleSpecInput)")
    val result = findSolution(input, 1000, 100)

    Then("Result is expected")
    result shouldBe 711480L
  }

  "findQuantumSolution" should "find result from spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)

    When("findQuantumSolution(puzzleSpecInput)")
    val result = findQuantumSolution(input, 21)

    Then("Result is 444356092776315")
    result shouldBe 444356092776315L
  }

  "findQuantumSolution" should "find result from input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleInput)

    When("findQuantumSolution(puzzleSpecInput)")
    val result = findQuantumSolution(input, 21)

    Then("Result is expected")
    result shouldBe 265845890886828L
  }
}
