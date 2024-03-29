package com.kensai.aoc.aoc2022

import com.kensai.aoc.lib.Geo.Point2D

object Day09 {

  sealed trait Direction {
    def move(point: Point2D, step: Int): Point2D = this match {
      case Up    => point.plusY(step)
      case Down  => point.minusY(step)
      case Right => point.plusX(step)
      case Left  => point.minusX(step)
    }
  }
  case object Up    extends Direction
  case object Down  extends Direction
  case object Left  extends Direction
  case object Right extends Direction
  case class MoveInstruction(direction: Direction, steps: Int)

  case class Rope(ropeNumber: Int, currentPos: Point2D, previousPositions: Set[Point2D]) {

    def move(newPos: Point2D): Rope =
      Rope(ropeNumber, newPos, previousPositions + newPos)
  }
  object Rope {
    def apply(ropeNumber: Int, currentPos: Point2D): Rope =
      Rope(ropeNumber, currentPos, Set(currentPos))
  }

  case class InputDay9(headPosition: Point2D, tailPosition: Point2D, instructions: Seq[MoveInstruction])

  private val upRegex    = """U (\d+)""".r
  private val downRegex  = """D (\d+)""".r
  private val leftRegex  = """L (\d+)""".r
  private val rightRegex = """R (\d+)""".r
  def parse(lines: Seq[String]): InputDay9 = {
    val instructions = lines.collect {
      case str if str.nonEmpty => str.trim
    }.map {
      case upRegex(countStr)    => MoveInstruction(Up, countStr.toInt)
      case downRegex(countStr)  => MoveInstruction(Down, countStr.toInt)
      case leftRegex(countStr)  => MoveInstruction(Left, countStr.toInt)
      case rightRegex(countStr) => MoveInstruction(Right, countStr.toInt)
      case str                  => throw new IllegalArgumentException(s"Invalid instruction for line [$str]")
    }
    InputDay9(Point2D(0, 0), Point2D(0, 0), instructions)
  }

  def countTailPositions(input: InputDay9, nbRopes: Int): Int = {
    val endPositions = executeAll(input, nbRopes)
    endPositions(nbRopes - 1).previousPositions.size
  }

  def executeAll(input: InputDay9, nbRopes: Int): Map[Int, Rope] = {
    val init: Map[Int, Rope] = (0 to nbRopes)
      .map(i => (i, Rope(i, Point2D(0, 0))))
      .toMap
    input.instructions
      .foldLeft(init) { case (positions, instr) =>
        moveAll(positions, instr, nbRopes)
      }
  }

  private def doMoveRope(headPos: Point2D, tailPos: Point2D): Point2D = {
    val diffX = headPos.x - tailPos.x
    val diffY = headPos.y - tailPos.y
    val newTail =
      if (math.abs(diffX) <= 1 && math.abs(diffY) <= 1) // Every pos at distance 1 (including diagonal)
        tailPos
      else if (math.abs(diffX) >= 2 && math.abs(diffY) >= 2) // Every diagonal at distance of 2 (for part 2 only)
        Point2D(headPos.x - diffX / 2, headPos.y - diffY / 2)
      else if (diffX == 2) // And abs(diffY)==1
        headPos.minusX(1)
      else if (diffX == -2) // And abs(diffY)==1
        headPos.plusX(1)
      else if (diffY == 2) // And abs(diffX)==1
        headPos.minusY(1)
      else // And abs(diffX)==1
        headPos.plusY(1)

    newTail
  }

  def moveAll(initialPositions: Map[Int, Rope], instr: MoveInstruction, nbRopes: Int): Map[Int, Rope] =
    (0 until instr.steps).foldLeft(initialPositions) { case (startStepPositions, _) =>
      // Update Head manually
      val newHead                           = instr.direction.move(startStepPositions(0).currentPos, 1)
      val startStepPositionsWithHeadUpdated = startStepPositions + (0 -> startStepPositions(0).move(newHead))

      // Then update other tails according to rope-1
      (1 until nbRopes).foldLeft(startStepPositionsWithHeadUpdated) { case (acc, ropeNumber) =>
        val previousRopeHead = acc(ropeNumber - 1).currentPos
        val previousRopeTail = acc(ropeNumber).currentPos
        val newTail          = doMoveRope(previousRopeHead, previousRopeTail)
        acc + (ropeNumber -> acc(ropeNumber).move(newTail))
      }
    }

}
