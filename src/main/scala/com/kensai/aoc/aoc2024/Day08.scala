package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

object Day08 {

  case class Day8Input(maxX: Int, maxY: Int, antennas: Map[Char, Set[Point2D]])

  def parse(rows: Seq[String]): Day8Input = {
    val maxX = rows.head.length - 1
    val maxY = rows.size - 1
    val antenas = for {
      x <- 0 to maxX
      y <- 0 to maxY if rows(y)(x) != '.'
    } yield (rows(y)(x), Point2D(x, y))
    val groupedAntennas = antenas
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap
    Day8Input(maxX = maxX, maxY = maxY, antennas = groupedAntennas)
  }

  private def generateAllPairs(antennas: Seq[Point2D]): Seq[(Point2D, Point2D)] =
    for {
      i <- 0 to antennas.size - 2
      j <- i + 1 to antennas.size - 1
    } yield (antennas(i), antennas(j))

  def computeAntiNodes(maxX: Int, maxY: Int, antennas: Set[Point2D]): Set[Point2D] = {
    val allPairs = generateAllPairs(antennas.toSeq)
    allPairs
      .flatMap { case (left, right) =>
        // Compute vectors 2(left->right)  and 2(right->left)
        val p2Symmetry = Point2D(2 * left.x - right.x, 2 * left.y - right.y)
        val p1Symmetry = Point2D(2 * right.x - left.x, 2 * right.y - left.y)
        Seq(p1Symmetry, p2Symmetry)
      }
      .toSet                                                           // Keep distinct
      .filter(p => p.x <= maxX && p.y <= maxY && p.x >= 0 && p.y >= 0) // Filter out-of-grid
  }

  def countAntiNodes(input: Day8Input): Int =
    input.antennas
      .flatMap { case (_, antennas) => computeAntiNodes(input.maxX, input.maxY, antennas) }
      .toSet
      .size

  def computeResonantAntiNodes(maxX: Int, maxY: Int, antennas: Set[Point2D]): Set[Point2D] = {
    val allPairs = generateAllPairs(antennas.toSeq)
    allPairs
      .flatMap { case (left, right) =>
        (1 to math.max(maxX, maxY))
          .flatMap { i =>
            val p2Symmetry = left + (right - left) * i
            val p1Symmetry = left + (right - left) * -i
            Seq(p1Symmetry, p2Symmetry)
          } ++
          Set(left, right)
      }
      .toSet                                                           // Keep distinct
      .filter(p => p.x <= maxX && p.y <= maxY && p.x >= 0 && p.y >= 0) // Filter out-of-grid
  }

  def countResonantAntiNodes(input: Day8Input): Int =
    input.antennas
      .flatMap { case (_, antennas) => computeResonantAntiNodes(input.maxX, input.maxY, antennas) }
      .toSet
      .size
}
