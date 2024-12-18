package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec


object Day18 {

  case class Day18Input(max: Int, bytesToFall: Seq[Point2D])

  def parse(rows: Seq[String], max: Int): Day18Input = {
    val bytes = rows.map{s =>
      val split = s.split(",")
      Point2D(split.head.toInt, split(1).toInt)
    }
    Day18Input(max, bytes)
  }

  def minStepsAfter(input: Day18Input, rounds: Int): Option[Int] = {
    val grid = input.bytesToFall.take(rounds).toSet
    val start = Point2D(0, 0)
    val end = Point2D(input.max, input.max)
    val result = computeMinSteps(input.max, grid, end, Map(), Seq((start, 0, Set())))
    result.map(_._1)
  }

  @tailrec
  def computeMinSteps(max: Int, grid: Set[Point2D], end: Point2D, seen: Map[Point2D, Int], todo: Seq[(Point2D, Int, Set[Point2D])]): Option[(Int, Set[Point2D])] = {
    if (todo.isEmpty){
      None
    } else {
      val (current, count, path) = todo.head
      if (current == end)
        Some((count, path))
      else if (seen.contains(current) && seen(current) <= count) // already seen a better path
        computeMinSteps(max, grid, end, seen, todo.tail)
      else {
        val nextPoints = current
          .nextPoints()
          .filterNot(grid.contains)
          .filter(p => p.x >= 0 && p.x <= max)
          .filter(p => p.y >= 0 && p.y <= max)
          .map(p => (p, count + 1, path + current))
        val nextTodo = (todo.tail ++ nextPoints ).sortBy(_._2) // Keep smallest first to find solution first
        val nextSeen = if (seen.contains(current)) seen.updated(current, count) else seen + (current -> count)
        computeMinSteps(max, grid, end, nextSeen, nextTodo)
      }
    }
  }

  def minStepsAfterRemoval(input: Day18Input, minRounds: Int): Point2D = {
    val start = Point2D(0, 0)
    val end = Point2D(input.max, input.max)
    val initialGrid = input.bytesToFall.take(minRounds).toSet
    val initialStep = computeMinSteps(input.max, initialGrid, end, Map(), Seq((start, 0, Set(start))))
    doMinStepsAfterRemoval(input, end, minRounds + 1, initialStep.get._2)
  }

  @tailrec
  private def doMinStepsAfterRemoval(input: Day18Input, end: Point2D, round: Int, currentBestPath: Set[Point2D]): Point2D = {
    val newByte = input.bytesToFall(round - 1)
    if (currentBestPath.contains(newByte)) {
      // New byte is on the best path
      // Recompute best path
      val start = Point2D(0, 0)
      val grid = input.bytesToFall.take(round).toSet
      val result = computeMinSteps(input.max, grid, end, Map(), Seq((start, 0, Set(start))))
      if (result.isDefined) // If there is a new best path => continue
        doMinStepsAfterRemoval(input, end, round + 1, result.get._2)
      else // If there is no best path, the new byte is the blocking one
        newByte
    } else {
      // New byte is not on the best path => No need to recompute
      doMinStepsAfterRemoval(input, end, round + 1, currentBestPath)
    }
  }

}
