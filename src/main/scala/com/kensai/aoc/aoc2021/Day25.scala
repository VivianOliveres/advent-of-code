package com.kensai.aoc.aoc2021

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec

object Day25 {

  case class InputDay25(stepNumber: Int, eastSeaCuCumber: Set[Point2D], southSeaCuCumber: Set[Point2D], maxX: Int, maxY: Int) {

    def hasNotChanged(other: InputDay25): Boolean =
      eastSeaCuCumber == other.eastSeaCuCumber && southSeaCuCumber == other.southSeaCuCumber
  }

  def parse(lines: Seq[String]): InputDay25 = {
    val (eastSeaCuCumber, southSeaCuCumber) = lines.zipWithIndex
      .map { case (line, rowNumber) =>
        line.zipWithIndex
          .map { case (cell, columnNumber) =>
            cell match {
              case 'v' => (None, Some(Point2D(columnNumber, rowNumber)))
              case '>' => (Some(Point2D(columnNumber, rowNumber)), None)
              case _   => (None, None)
            }
          }
          .foldLeft((Set.empty[Point2D], Set.empty[Point2D])) { case (acc, (maybeEast, maybeSouth)) =>
            (acc._1 ++ maybeEast.toSet, acc._2 ++ maybeSouth.toSet)
          }
      }
      .foldLeft((Set.empty[Point2D], Set.empty[Point2D])) { case (acc, (easts, souths)) =>
        (acc._1 ++ easts, acc._2 ++ souths)
      }
    val (maxX, maxY) = (eastSeaCuCumber ++ southSeaCuCumber).foldLeft((0, 0)) { case ((maxX, maxY), Point2D(x, y)) =>
      (if (x > maxX) x else maxX, if (y > maxY) y else maxY)
    }
    InputDay25(0, eastSeaCuCumber, southSeaCuCumber, maxX, maxY)
  }

  def executeStep(input: InputDay25): InputDay25 = {
    val allPositions = input.eastSeaCuCumber ++ input.southSeaCuCumber
    val (allToRemove1, allToAdd1) = input.eastSeaCuCumber
      .map(point => (point, computeNextEastPosition(point, input.maxX)))
      .filterNot(tuple => allPositions.contains(tuple._2))
      .foldLeft((Set.empty[Point2D], Set.empty[Point2D])) { case ((accToRemove, accToAdd), (toRemove, toAdd)) =>
        (accToRemove + toRemove, accToAdd + toAdd)
      }

    val midInput = input.copy(eastSeaCuCumber = input.eastSeaCuCumber -- allToRemove1 ++ allToAdd1)

    val midAllPositions = midInput.eastSeaCuCumber ++ midInput.southSeaCuCumber
    val (allToRemove2, allToAdd2) = midInput.southSeaCuCumber
      .map(point => (point, computeNextSouthPosition(point, input.maxY)))
      .filterNot(tuple => midAllPositions.contains(tuple._2))
      .foldLeft((Set.empty[Point2D], Set.empty[Point2D])) { case ((accToRemove, accToAdd), (toRemove, toAdd)) =>
        (accToRemove + toRemove, accToAdd + toAdd)
      }

    midInput.copy(stepNumber = midInput.stepNumber + 1, southSeaCuCumber = midInput.southSeaCuCumber -- allToRemove2 ++ allToAdd2)
  }

  private def computeNextEastPosition(currentPosition: Point2D, maxX: Int): Point2D =
    if (currentPosition.x == maxX)
      currentPosition.copy(x = 0)
    else
      currentPosition.copy(x = currentPosition.x + 1)

  private def computeNextSouthPosition(currentPosition: Point2D, maxY: Int): Point2D =
    if (currentPosition.y == maxY)
      currentPosition.copy(y = 0)
    else
      currentPosition.copy(y = currentPosition.y + 1)

  @tailrec
  def computeLastStepWithoutAnythingMoving(input: InputDay25): Int = {
    val newInput = executeStep(input)
    if (input.hasNotChanged(newInput))
      newInput.stepNumber
    else
      computeLastStepWithoutAnythingMoving(newInput)
  }

}
