package com.kensai.aoc

import com.kensai.aoc.Day11._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputFile("src/test/resources/Day11.input")
  private lazy val state0Inputs = readInputFile("src/test/resources/Day11SpecState0.input")

  private lazy val state1Part1Inputs = readInputFile("src/test/resources/Day11SpecState1Part1.input")
  private lazy val state2Part1Inputs = readInputFile("src/test/resources/Day11SpecState2Part1.input")
  private lazy val state3Part1Inputs = readInputFile("src/test/resources/Day11SpecState3Part1.input")
  private lazy val state4Part1Inputs = readInputFile("src/test/resources/Day11SpecState4Part1.input")
  private lazy val state5Part1Inputs = readInputFile("src/test/resources/Day11SpecState5Part1.input")

  private lazy val state1Part2Inputs = readInputFile("src/test/resources/Day11SpecState1Part2.input")
  private lazy val state2Part2Inputs = readInputFile("src/test/resources/Day11SpecState2Part2.input")
  private lazy val state3Part2Inputs = readInputFile("src/test/resources/Day11SpecState3Part2.input")
  private lazy val state4Part2Inputs = readInputFile("src/test/resources/Day11SpecState4Part2.input")
  private lazy val state5Part2Inputs = readInputFile("src/test/resources/Day11SpecState5Part2.input")

  "parse empty" should "return empty board of 3" in {
    Given(s"Input is spec")
    val i =
      """
        |...
        |""".stripMargin

    val result = parse(i)

    result shouldBe Board(1, 3, Map())
  }

  "parse [L.#]" should "return corresponding board" in {
    Given(s"Input is spec")
    val i =
      """
        |L.#
        |""".stripMargin

    val result = parse(i)

    result shouldBe Board(1, 3, Map(Pos(0, 0) -> 'L', Pos(0, 2) -> '#'))
  }

  "parse multiple lines" should "return corresponding board" in {
    Given(s"Input is spec")
    val i =
      """
        |L.#
        |..L
        |""".stripMargin

    val result = parse(i)

    result shouldBe Board(2, 3, Map(Pos(0, 0) -> 'L', Pos(0, 2) -> '#', Pos(1, 2) -> 'L'))
  }


  "extractAdjacent of empty" should "return empty" in {
    Given(s"Input is spec")
    val i =
      """
        |...
        |""".stripMargin

    val board = parse(i)

    val result1 = extractAdjacent(Pos(0, 0), board)
    result1 shouldBe List.empty[Char]

    val result2 = extractAdjacent(Pos(0, 1), board)
    result2 shouldBe List.empty[Char]

    val result3 = extractAdjacent(Pos(0, 2), board)
    result3 shouldBe List.empty[Char]
  }

  "extractAdjacent of [L.#]" should "return corresponding value" in {
    Given(s"Input is spec")
    val i =
      """
        |L.#
        |""".stripMargin

    val board = parse(i)

    val result1 = extractAdjacent(Pos(0, 0), board)
    result1 shouldBe List()

    val result2 = extractAdjacent(Pos(0, 1), board)
    result2 shouldBe List('#', 'L')

    val result3 = extractAdjacent(Pos(0, 2), board)
    result3 shouldBe List()
  }

  "extractAdjacent of all directions" should "return corresponding value" in {
    Given(s"Input is spec")
    val i =
      """
        |LL#
        |L.#
        |L##
        |""".stripMargin

    val board = parse(i)

    val result = extractAdjacent(Pos(1, 1), board).sorted
    result shouldBe List('L', 'L', 'L', 'L', '#', '#', '#', '#').sorted
  }

  "computeSwitch of # for 8#" should "return L" in {
    Given(s"Input is spec")
    val i =
      """
        |###
        |###
        |###
        |""".stripMargin

    val board = parse(i)

    val result = computeSwitch(Pos(1, 1), board, 4, extractAdjacent)
    result shouldBe Some((Pos(1, 1), 'L'))
  }

  "computeSwitch of # for 3#" should "return None" in {
    Given(s"Input is spec")
    val i =
      """
        |###
        |.#.
        |LLL
        |""".stripMargin

    val board = parse(i)

    val result = computeSwitch(Pos(1, 1), board, 4, extractAdjacent)
    result shouldBe None
  }

  "computeSwitch of L for 8#" should "return None" in {
    Given(s"Input is spec")
    val i =
      """
        |###
        |#L#
        |###
        |""".stripMargin

    val board = parse(i)

    val result = computeSwitch(Pos(1, 1), board, 4, extractAdjacent)
    result shouldBe None
  }

  "doComputeNextBoard of L for 8#" should "return None" in {
    Given(s"Input is spec")
    val i =
      """
        |...
        |.L.
        |...
        |""".stripMargin

    val board = parse(i)

    val result = doComputeNextBoard(board, 4, extractAdjacent)
    result shouldBe Board(3, 3, Map(Pos(1, 1) -> '#'))
  }

  "doComputeNextBoard of Step0" should "return Step1" in {
    Given(s"Input is spec")
    val i = state0Inputs
    val board = parse(i)

    val result = doComputeNextBoard(board, 4, extractAdjacent)

    val expected = parse(state1Part1Inputs)
    result shouldBe expected
  }

  "doComputeNextBoard of Step1" should "return Step2" in {
    Given(s"Input is spec")
    val i = state1Part1Inputs
    val board = parse(i)

    val result = doComputeNextBoard(board, 4, extractAdjacent)

    val expected = parse(state2Part1Inputs)
    result shouldBe expected
  }

  "doComputeNextBoard of Step2" should "return Step3" in {
    Given(s"Input is spec")
    val i = state2Part1Inputs
    val board = parse(i)

    val result = doComputeNextBoard(board, 4, extractAdjacent)

    val expected = parse(state3Part1Inputs)
    result shouldBe expected
  }

  "doComputeNextBoard of Step3" should "return Step4" in {
    Given(s"Input is spec")
    val i = state3Part1Inputs
    val board = parse(i)

    val result = doComputeNextBoard(board, 4, extractAdjacent)

    val expected = parse(state4Part1Inputs)
    result shouldBe expected
  }

  "doComputeNextBoard of Step4" should "return Step5" in {
    Given(s"Input is spec")
    val i = state4Part1Inputs
    val board = parse(i)

    val result = doComputeNextBoard(board, 4, extractAdjacent)

    val expected = parse(state5Part1Inputs)
    result shouldBe expected
  }

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

  "doComputeNextBoard for visible of L to 5#" should "return correct solution" in {
    Given(s"Input is spec")
    val i =
      """
        |L.#.L
        |.....
        |#.L.#
        |.....
        |L.#.L
        |""".stripMargin
    val board = parse(i)

    val result = doComputeNextBoard(board, 5, extractVisibleAdjacent)

    result shouldBe board
  }

  "doComputeNextBoard for visible of L only" should "return #" in {
    Given(s"Input is spec")
    val i =
      """
        |...
        |.L.
        |...
        |""".stripMargin
    val board = parse(i)

    val result = doComputeNextBoard(board, 5, extractVisibleAdjacent)

    result shouldBe board.updated(Pos(1, 1), '#')
  }

  "doComputeNextBoard for visible of 2#" should "return 2L" in {
    Given(s"Input is spec")
    val i =
      """
        |#.L
        |...
        |..#
        |""".stripMargin
    val board = parse(i)

    val result = doComputeNextBoard(board, 1, extractVisibleAdjacent)

    val expected = parse(
      """
        |L.L
        |...
        |..L
        |""".stripMargin
    )
    result shouldBe expected
  }

  "doComputeNextBoard for visible of 4#" should "return unaltered" in {
    Given(s"Input is spec")
    val i =
      """
        |###
        |.#.
        |..#
        |""".stripMargin
    val board = parse(i)

    val result = doComputeNextBoard(board, 5, extractVisibleAdjacent)

    result shouldBe board
  }

  "doComputeNextBoard for visible of border" should "return L" in {
    Given(s"Input is spec")
    val i =
      """
        |##
        |##
        |##
        |""".stripMargin
    val board = parse(i)

    val result = doComputeNextBoard(board, 5, extractVisibleAdjacent)

    val expected = parse(
      """
        |##
        |LL
        |##
        |""".stripMargin
    )
    result shouldBe expected
  }

  "doComputeNextBoard for visible of all#" should "return opposite" in {
    Given(s"Input is spec")
    val i =
      """
        |###
        |###
        |###
        |""".stripMargin
    val board = parse(i)

    val result = doComputeNextBoard(board, 5, extractVisibleAdjacent)

    val expected = parse(
      """
        |#L#
        |LLL
        |#L#
        |""".stripMargin
    )
    result shouldBe expected
  }

  "doComputeNextBoard for visible of Step0" should "return Step1" in {
    Given(s"Input is spec")
    val i = state0Inputs
    val board = parse(i)

    val result = doComputeNextBoard(board, 5, extractVisibleAdjacent)

    val expected = parse(state1Part2Inputs)
    result shouldBe expected
  }

  "doComputeNextBoard for visible of Step1" should "return Step2" in {
    Given(s"Input is spec")
    val i = state1Part1Inputs
    val board = parse(i)

    val result = doComputeNextBoard(board, 5, extractVisibleAdjacent)

    val expected = parse(state2Part2Inputs)
    result shouldBe expected
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
