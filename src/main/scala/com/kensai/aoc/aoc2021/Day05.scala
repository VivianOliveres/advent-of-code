package com.kensai.aoc.aoc2021

import com.kensai.aoc.lib.Geo.Point2D

object Day05 {

  case class VentLine(x1: Int, y1: Int, x2: Int, y2: Int) {
    def generatePoints: Seq[Point2D] =
      if (x1 == x2)
        range(y1, y2).map(y => Point2D(x1, y))
      else if (y1 == y2)
        range(x1, x2).map(x => Point2D(x, y1))
      else
        for {
          (x, y) <- range(x1, x2) zip range(y1, y2)
        } yield Point2D(x, y)

    private def range(val1: Int, val2: Int): Range = {
      val step = if (val1 < val2) 1 else -1
      val1 to val2 by step
    }
  }

  private val lineRegex = """(\d+),(\d+) -> (\d+),(\d+)""".r
  private def parse(inputs: Seq[String]): Seq[VentLine] =
    inputs.filterNot(_.isEmpty).map(_.trim).flatMap { input =>
      input match {
        case lineRegex(x1, y1, x2, y2) =>
          Some(VentLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        case _ => None
      }
    }

  /**
    * Count the number of dangerous points (ie: points crossed by at least to VentLine).
    * But count only the Vertical and Horizontal lines.
    */
  def countVHDangerousPoints(inputs: Seq[String]): Int =
    countDangerousPoints(inputs, line => line.x1 == line.x2 || line.y1 == line.y2)

  private def countDangerousPoints(inputs: Seq[String], filter: VentLine=>Boolean): Int = {
    val lines = parse(inputs).filter(filter)
    val points = lines.flatMap(_.generatePoints)
    val countByPoint =
      points.groupBy(point => (point.x, point.y)).map{case (key, value) => (key, value.size)}
    countByPoint.count(_._2 > 1)
  }

  /**
    * Count the number of dangerous points (ie: points crossed by at least to VentLine).
    * Count Vertical, Horizontal and Diagonal lines.
    */
  def countAllDangerousPoints(inputs: Seq[String]): Int = {
    countDangerousPoints(inputs, _ => true)
  }

}
