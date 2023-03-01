package com.kensai.aoc.aoc2020

import Day12._
import Day12.Direction._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day12PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen {

  property("parse should return valid Command") {
    val inputs = Table(
      "Test parameters",
      SimpleTestParam("F10", Forward(10)),
      SimpleTestParam("N3", North(3)),
      SimpleTestParam("S7", South(7)),
      SimpleTestParam("E159", East(159)),
      SimpleTestParam("W357", West(357)),
      SimpleTestParam("R90", Right(90)),
      SimpleTestParam("L180", Left(180))
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"parse(${param.input})")
      val result = parse(List(param.input))

      Then(s"result should be ${param.expectedResult}")
      result shouldBe List(param.expectedResult)
    }
  }

  property("move should return valid Position") {

    case class MoveTestParam(
        initialPosition: Pos,
        command: Command,
        expectedPosition: Pos
    )

    val inputs = Table(
      "Test parameters",
      MoveTestParam(Pos(0, 0, E), Forward(10), Pos(10, 0, E)),
      MoveTestParam(Pos(10, 0, E), North(3), Pos(10, 3, E)),
      MoveTestParam(Pos(10, 3, E), Forward(7), Pos(17, 3, E)),
      MoveTestParam(Pos(17, 3, E), Right(90), Pos(17, 3, S)),
      MoveTestParam(Pos(17, 3, S), Forward(11), Pos(17, -8, S))
    )

    forAll(inputs) { param =>
      Given(
        s"Initial position is ${param.initialPosition} and command is ${param.command}"
      )

      When(s"move(${param.initialPosition}, ${param.command})")
      val result = move(param.initialPosition, List(param.command))

      Then(s"result should be ${param.expectedPosition}")
      result shouldBe param.expectedPosition
    }
  }

  property("doMove2 should return valid Position and Waypoint") {

    case class Move2TestParam(
        initialPosition: Pos,
        initialWaypoint: Pos,
        command: Command,
        expectedPosition: Pos,
        expectedWaypoint: Pos
    )

    val inputs = Table(
      "Test parameters",
      Move2TestParam(
        Pos(0, 0, E),
        Pos(10, 1, N),
        Forward(10),
        Pos(100, 10, E),
        Pos(10, 1, N)
      ),
      Move2TestParam(
        Pos(100, 10, E),
        Pos(10, 1, N),
        North(3),
        Pos(100, 10, E),
        Pos(10, 4, N)
      ),
      Move2TestParam(
        Pos(100, 10, E),
        Pos(10, 4, N),
        Forward(7),
        Pos(170, 38, E),
        Pos(10, 4, N)
      ),
      Move2TestParam(
        Pos(170, 38, E),
        Pos(10, 4, N),
        Right(90),
        Pos(170, 38, E),
        Pos(4, -10, N)
      )
    )

    forAll(inputs) { param =>
      Given(
        s"Pos[${param.initialPosition}] Waypoint[${param.initialWaypoint}] Command[${param.command}]"
      )

      When(
        s"move(${param.initialPosition}, param.initialWaypoint, ${param.command})"
      )
      val result =
        doMove2(param.initialPosition, param.initialWaypoint, param.command)

      Then(
        s"result should be Pos[${param.expectedPosition}] Waypoint[${param.expectedWaypoint}]"
      )
      result shouldBe (param.expectedPosition, param.expectedWaypoint)
    }
  }
}
