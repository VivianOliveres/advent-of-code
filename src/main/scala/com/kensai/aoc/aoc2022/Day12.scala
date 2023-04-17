package com.kensai.aoc.aoc2022

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec
import scala.collection.mutable

object Day12 {

  sealed trait Cell {
    def value: Int
  }
  case class ValueCell(rep: Char) extends Cell {
    override val value: Int = rep.toInt
  }
  case object StartCell extends Cell {
    override def value: Int = 'a'.toInt
  }
  case object EndCell extends Cell {
    override def value: Int = 'z'.toInt
  }

  def parse(lines: Seq[String]): Map[Point2D, Cell] =
    lines
      .map(_.trim)
      .filterNot(_.isEmpty)
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (c, x) =>
          val cell = if (c == 'S') StartCell else if (c == 'E') EndCell else ValueCell(c)
          Point2D(x, y) -> cell
        }
      }
      .toMap

  def computeShortestPath(cells: Map[Point2D, Cell]): Int = {
    val startPos      = cells.find(_._2 == StartCell).map(_._1).get
    val bestPositions = mutable.Map(startPos -> 0)
    val nextPositions = mutable.PriorityQueue((startPos, 0))(Ordering.by(b => -b._2))
    val maybeResult   = advance(cells, bestPositions, nextPositions)
    maybeResult.get
  }

  @tailrec
  private def advance(
      cells: Map[Point2D, Cell],
      bestPositions: mutable.Map[Point2D, Int],
      nextPositions: mutable.PriorityQueue[(Point2D, Int)]
    ): Option[Int] = {
    if (nextPositions.isEmpty)
      None
    else {
      val (currentPos, stepsCount) = nextPositions.dequeue()
      val newStepsCount = stepsCount + 1
      if (cells(currentPos) == EndCell) {
        Some(stepsCount)
      } else if (bestPositions(currentPos) < stepsCount) {
        advance(cells, bestPositions, nextPositions)
      } else {
        val currentValue = cells(currentPos).value
        Seq(currentPos.minusX(1), currentPos.plusX(1), currentPos.minusY(1), currentPos.plusY(1))
          .filter(cells.contains) // Is in grid
          .filter(newPos => cells(newPos).value <= currentValue + 1) // Neighbour has no more than value+1
          .filter(newPos => !bestPositions.contains(newPos) || bestPositions(newPos) > newStepsCount) // Didn't already find a shortest path
          .foreach { newPos =>
            bestPositions.put(newPos, newStepsCount)
            nextPositions.enqueue((newPos, newStepsCount))
            ()
          }

        advance(cells, bestPositions, nextPositions)
      }
    }
  }

  def computeAnyShortestPath(cells: Map[Point2D, Cell]): Int = {
    val startPos = cells.find(_._2 == StartCell).map(_._1).get
    val otherStartingCells = cells.filter(_._2 == ValueCell('a')).keys.toSeq
    val results = (otherStartingCells :+ startPos).flatMap{ pos =>
      val bestPositions = mutable.Map(pos -> 0)
      val nextPositions = mutable.PriorityQueue((pos, 0))(Ordering.by(b => -b._2))
      advance(cells, bestPositions, nextPositions)
    }
    results.min
  }

}
