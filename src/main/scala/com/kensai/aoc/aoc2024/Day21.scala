package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

import scala.collection.mutable

object Day21 {

  private val numpad: Map[Char, Point2D] = Map(
    '7' -> Point2D(0, 0), '8' -> Point2D(0, 1), '9' -> Point2D(0, 2),
    '4' -> Point2D(1, 0), '5' -> Point2D(1, 1), '6' -> Point2D(1, 2),
    '1' -> Point2D(2, 0), '2' -> Point2D(2, 1), '3' -> Point2D(2, 2),
    '0' -> Point2D(3, 1), 'A' -> Point2D(3, 2)
  )

  private val numpadInv: Map[Point2D, Char] = numpad.map(_.swap)

  private val dirpad: Map[Char, Point2D] = Map(
    '^' -> Point2D(0, 1), 'A' -> Point2D(0, 2),
    '<' -> Point2D(1, 0), 'v' -> Point2D(1, 1), '>' -> Point2D(1, 2)
  )

  private val dirpadInv: Map[Point2D, Char] = dirpad.map(_.swap)

  private val dirs: Map[Char, Point2D] = Map(
    '^' -> Point2D(-1, 0), 'v' -> Point2D(1, 0),
    '<' -> Point2D(0, -1), '>' -> Point2D(0, 1)
  )

  private val cache: mutable.Map[(Int, Char, Char, Int), Long] = mutable.Map()

  // TODO: improve
  def func(robotId: Int, currentKey: Char, destKey: Char, totalRobots: Int): Long = {
    cache.getOrElseUpdate((robotId, currentKey, destKey, totalRobots), {
      val (pad, padInv) = if (robotId == 0) (numpad, numpadInv) else (dirpad, dirpadInv)
      val currentPos = pad(currentKey)
      val destPos = pad(destKey)
      val delta = destPos - currentPos
      if (robotId == totalRobots - 1) {
        return Math.abs(delta.x) + Math.abs(delta.y) + 1L
      }
      val seq = mutable.ArrayBuffer[Char]()
      for (_ <- 0 until Math.abs(delta.x)) {
        seq.append(if (delta.x < 0) '^' else 'v')
      }
      for (_ <- 0 until Math.abs(delta.y)) {
        seq.append(if (delta.y < 0) '<' else '>')
      }
      if (seq.isEmpty) return 1L
      val candidates = mutable.ArrayBuffer[Long]()
      for (r <- seq.permutations) {
        var pos = currentPos
        var steps = 0L
        var valid = true
        for (i <- r.indices) {
          steps += func(robotId + 1, if (i == 0) 'A' else r(i - 1), r(i), totalRobots)
          pos += dirs(r(i))
          if (!padInv.contains(pos)) {
            valid = false
          }
        }
        if (valid) {
          steps += func(robotId + 1, r.last, 'A', totalRobots)
          candidates.append(steps)
        }
      }
      candidates.min
    })
  }

  private def doComputeComplexity(input: Seq[String], nbRobots: Int): Long = {
    val complexities = for {
      code <- input
    } yield {
      val init = func(0, 'A', code.head, nbRobots)
      val complexity = (1 until code.length).foldLeft(init) {case (acc, i) =>
        acc + func(0, code(i - 1), code(i), nbRobots)
      }
      complexity * code.init.toLong
    }
    complexities.sum
  }

  def complexity(input: Seq[String]): Int =
    doComputeComplexity(input, 3).toInt

  def complexity2(input: Seq[String]): Long =
    doComputeComplexity(input, 26)

}