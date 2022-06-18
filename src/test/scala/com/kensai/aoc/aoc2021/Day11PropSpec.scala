package com.kensai.aoc.aoc2021

import Day11._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen
    with Day11Fixtures {

  property("parse should return valid Board") {
    val inputs = Table(
      "Test parameters",
      SimpleTestParam("...", Board(1, 3, Map())),
      SimpleTestParam(
        "L.#",
        Board(1, 3, Map(Pos(0, 0) -> 'L', Pos(0, 2) -> '#'))
      ),
      SimpleTestParam(
        "L.#\n..L\n",
        Board(2, 3, Map(Pos(0, 0) -> 'L', Pos(0, 2) -> '#', Pos(1, 2) -> 'L'))
      )
    )

    forAll(inputs) { param =>
      Given(s"Input is \n${param.input}")

      When(s"parse(input)")
      val result = parse(param.input)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("extractAdjacent should return valid List") {

    case class ExtractAdjacentTestParam(
        input: String,
        pos: Pos,
        expectedResult: List[Char]
    )

    val inputs = Table(
      "Test parameters",
      ExtractAdjacentTestParam("...", Pos(0, 0), List.empty[Char]),
      ExtractAdjacentTestParam("...", Pos(0, 1), List.empty[Char]),
      ExtractAdjacentTestParam("...", Pos(0, 2), List.empty[Char]),
      ExtractAdjacentTestParam("L.#", Pos(0, 0), List.empty[Char]),
      ExtractAdjacentTestParam("L.#", Pos(0, 1), List('#', 'L')),
      ExtractAdjacentTestParam("L.#", Pos(0, 2), List.empty[Char]),
      ExtractAdjacentTestParam(
        "LL#\nL.#\nL##\n",
        Pos(1, 1),
        List('L', 'L', 'L', 'L', '#', '#', '#', '#')
      )
    )

    forAll(inputs) { param =>
      Given(s"Input is \n${param.input}")
      val board = parse(param.input)

      When(s"parse(${param.pos}, input)")
      val result = extractAdjacent(param.pos, board).sorted

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult.sorted
    }
  }

  property(
    "computeSwitch should return new char for count=4 and extractAdjacent"
  ) {

    case class ComputeSwitchTestParam(
        input: String,
        pos: Pos,
        expectedResult: Option[(Pos, Char)]
    )

    val inputs = Table(
      "Test parameters",
      ComputeSwitchTestParam("###\n#L#\n###", Pos(1, 1), None),
      ComputeSwitchTestParam(
        "###\n###\n###",
        Pos(1, 1),
        Some((Pos(1, 1), 'L'))
      ),
      ComputeSwitchTestParam("###\n.#.\nLLL", Pos(1, 1), None)
    )

    forAll(inputs) { param =>
      Given(s"Input is \n${param.input}")
      val board = parse(param.input)

      When(s"parse(${param.pos}, input)")
      val result = computeSwitch(param.pos, board, 4, extractAdjacent)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("doComputeNextBoard should return valid board according to specs") {

    val inputs = Table(
      "Test parameters",
      SimpleTestParam(state1Part1Inputs, state2Part1Inputs),
      SimpleTestParam(state2Part1Inputs, state3Part1Inputs),
      SimpleTestParam(state3Part1Inputs, state4Part1Inputs),
      SimpleTestParam(state4Part1Inputs, state5Part1Inputs)
    )

    forAll(inputs) { param =>
      Given(s"Input is \n${param.input}")
      val board = parse(param.input)

      When(s"parse(${param.pos}, input)")
      val result = doComputeNextBoard(board, 4, extractAdjacent)

      val expectedResult = parse(param.expectedResult)
      Then(s"result should be $expectedResult")
      result shouldBe expectedResult
    }
  }
  //
  property("doComputeNextBoard should return valid adjacent") {

    case class ComputeNextBoardTestParam(
        input: String,
        expectedCount: Int,
        expectedResult: String
    )

    val inputs = Table(
      "Test parameters",
      ComputeNextBoardTestParam(
        "L.#.L\n.....\n#.L.#\n.....\nL.#.L\n",
        5,
        "L.#.L\n.....\n#.L.#\n.....\nL.#.L\n"
      ), // Same board
      ComputeNextBoardTestParam("...\n.L.\n...\n", 5, "...\n.#.\n...\n"),
      ComputeNextBoardTestParam("#.L\n...\n..#\n", 1, "L.L\n...\n..L\n"),
      ComputeNextBoardTestParam(
        "###\n.#.\n..#\n",
        5,
        "###\n.#.\n..#\n"
      ), // Same board
      ComputeNextBoardTestParam("##\n##\n##\n", 5, "##\nLL\n##\n"),
      ComputeNextBoardTestParam("###\n###\n###\n", 5, "#L#\nLLL\n#L#\n"),
      ComputeNextBoardTestParam(state0Inputs, 5, state1Part2Inputs),
      ComputeNextBoardTestParam(state1Part2Inputs, 5, state2Part2Inputs),
      ComputeNextBoardTestParam(state2Part2Inputs, 5, state3Part2Inputs),
      ComputeNextBoardTestParam(state3Part2Inputs, 5, state4Part2Inputs),
      ComputeNextBoardTestParam(state4Part2Inputs, 5, state5Part2Inputs)
    )

    forAll(inputs) { param =>
      Given(s"Input is \n${param.input}")
      val board = parse(param.input)

      When(s"doComputeNextBoard(${param.expectedCount}, input)")
      val result =
        doComputeNextBoard(board, param.expectedCount, extractVisibleAdjacent)

      val expectedResult = parse(param.expectedResult)
      Then(s"result should be $expectedResult")
      result shouldBe expectedResult
    }
  }
}
