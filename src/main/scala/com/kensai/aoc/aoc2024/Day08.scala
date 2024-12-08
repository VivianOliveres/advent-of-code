package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Pairs

object Day08 {

  case class Day8Input(maxX: Int, maxY: Int, antennas: Map[Char, Set[Point2D]])

  def parse(rows: Seq[String]): Day8Input = {
    val maxX = rows.head.length - 1
    val maxY = rows.size - 1

    val antennas = for {
      x <- 0 to maxX
      y <- 0 to maxY if rows(y)(x) != '.'
    } yield (rows(y)(x), Point2D(x, y))

    val groupedAntennas = antennas
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap

    Day8Input(maxX = maxX, maxY = maxY, antennas = groupedAntennas)
  }

  sealed trait AntiNodesGenerator extends ((Point2D, Point2D) => Set[Point2D])
  case object AntiNodesGeneratorPart1 extends AntiNodesGenerator {
    override def apply(left: Point2D, right: Point2D): Set[Point2D] =
      generate(left, right, 2)
  }
  case class AntiNodesGeneratorPart2(factor: Int) extends AntiNodesGenerator {
    override def apply(left: Point2D, right: Point2D): Set[Point2D] =
      (1 to factor).flatMap(generate(left, right, _)).toSet
  }

  def computeAntiNodes(maxX: Int, maxY: Int, antennas: Set[Point2D], generator: AntiNodesGenerator): Set[Point2D] = {
    Pairs.generateOrderedPairs(antennas.toSeq)
      .flatMap{case (left, right) => generator(left, right)}
      .toSet                                                           // Keep distinct
      .filter(p => p.x <= maxX && p.y <= maxY && p.x >= 0 && p.y >= 0) // Filter out-of-grid
  }

  def countAntiNodes(input: Day8Input, generator: AntiNodesGenerator): Int =
    input.antennas
      .flatMap { case (_, antennas) => computeAntiNodes(input.maxX, input.maxY, antennas, generator) }
      .toSet
      .size

  /**
   * Return two points on the line p1<->p2 distanced of k. <br>
   * If k == 1 then it returns p1 and p2.
   * @param p1: First point
   * @param p2: Second point
   * @param k >= 1
   * @return 2 points
   */
  private def generate(p1: Point2D, p2: Point2D, k: Int): Set[Point2D] =
    Set(
      (p2-p1) * k + p1,
      (p1-p2) * k + p2,
    )

}
