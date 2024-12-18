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
    computeMinSteps(input.max, grid, end, Map(), Seq((start, 0)))
  }

  @tailrec
  def computeMinSteps(max: Int, grid: Set[Point2D], end: Point2D, seen: Map[Point2D, Int], todo: Seq[(Point2D, Int)]): Option[Int] = {
    if (todo.isEmpty){
      None
    } else {
      val (current, count) = todo.head
      if (current == end)
        Some(count)
      else if (seen.contains(current) && seen(current) <= count) // already seen a better path
        computeMinSteps(max, grid, end, seen, todo.tail)
      else {
        val nextPoints = current
          .nextPoints()
          .filterNot(grid.contains)
          .filter(p => p.x >= 0 && p.x <= max)
          .filter(p => p.y >= 0 && p.y <= max)
          .map(p => (p, count + 1))
        val nextTodo = (todo.tail ++ nextPoints ).sortBy(_._2) // Keep smallest first to find solution first
        val nextSeen = if (seen.contains(current)) seen.updated(current, count) else seen + (current -> count)
        computeMinSteps(max, grid, end, nextSeen, nextTodo)
      }
    }
  }

  def minStepsAfterRemoval(input: Day18Input, minRounds: Int): Point2D = {
    val start = Point2D(0, 0)
    val end = Point2D(input.max, input.max)
    val lastByte = (minRounds until input.bytesToFall.size).find {i =>
      val grid = input.bytesToFall.take(i).toSet
      computeMinSteps(input.max, grid, end, Map(), Seq((start, 0))).isEmpty
    }
    input.bytesToFall(lastByte.get - 1)
  }

}
