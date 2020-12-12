package com.kensai.aoc

import com.kensai.aoc.Day11._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Spec extends AnyFlatSpec with GivenWhenThen with Day11Fixtures {

  "countOccupiedSeats for specs" should "return 37" in {
    Given(s"Input is spec")
    val board = parse(state0Inputs)

    val result = countOccupiedSeats(board)

    result shouldBe 37L
  }

  "countOccupiedSeats for puzzle" should "return solution" in {
    Given(s"Puzzle input")
    val board = parse(puzzleInputs)

    val result = countOccupiedSeats(board)

    result shouldBe 2481L
  }

  "extractVisibleAdjacent of all directions" should "return corresponding value" in {
    Given(s"Input is spec")
    val i =
      """
        |L.#.L
        |.....
        |#...#
        |.....
        |L.#.L
        |""".stripMargin

    val board = parse(i)

    val result = extractVisibleAdjacent(Pos(2, 2), board).sorted
    result shouldBe List('L', 'L', 'L', 'L', '#', '#', '#', '#').sorted
  }

  "countOccupiedSeats2 for spec" should "return 26" in {
    Given(s"Input is spec")
    val i = parse(state0Inputs)

    When(s"countOccupiedSeats2(input)")
    val result = countOccupiedSeats2(i)

    Then(s"Result is 26")
    result shouldBe 26L
  }

  "countOccupiedSeats2 for puzzle" should "return solution" in {
    Given(s"Puzzle input")
    val i = parse(puzzleInputs)

    When(s"countOccupiedSeats2(input)")
    val result = countOccupiedSeats2(i)

    Then(s"Result is 2232")
    result shouldBe 2227L
  }

}
