package com.kensai.aoc.aoc2022

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec

object Day14 {

  case class InputDay14(minX: Int, maxX: Int, maxY: Int, points: Set[Point2D], isPart2: Boolean) {
    def contains(point: Point2D): Boolean =
      if (isPart2)
        points.contains(point) || point.y == maxY
      else
        points.contains(point)

    def isOutside(point: Point2D): Boolean =
      if (isPart2)
        false
      else
        point.x < minX || point.x > maxX || point.y > maxY

    def addSand(sand: Point2D): InputDay14 =
      copy(points = points + sand)

    def part2: InputDay14 =
      copy(maxY = maxY + 2, isPart2 = true)
  }

  def parse(lines: Seq[String]): InputDay14 = {
    val allRocks = lines.filter(_.nonEmpty).map(doParse).foldLeft(Set.empty[Point2D])(_ ++ _)
    val minX     = allRocks.minBy(_.x).x
    val maxX     = allRocks.maxBy(_.x).x
    val maxY     = allRocks.maxBy(_.y).y
    InputDay14(minX, maxX, maxY, allRocks, false)
  }

  private def parsePoint(str: String): Point2D = {
    val splitPoint = str.split(",")
    Point2D(splitPoint.head.toInt, splitPoint(1).toInt)
  }
  private def doParse(line: String): Set[Point2D] = {
    val split     = line.split(" -> ").toSeq
    val headPoint = parsePoint(split.head)
    val (_, allNewPoints) = split.tail.foldLeft((headPoint, Set.empty[Point2D])) { case ((previousPoint, acc), str) =>
      val nextPoint = parsePoint(str)
      val newPoints = (for {
        x <- math.min(previousPoint.x, nextPoint.x) to math.max(previousPoint.x, nextPoint.x)
        y <- math.min(previousPoint.y, nextPoint.y) to math.max(previousPoint.y, nextPoint.y)
      } yield Point2D(x, y)).toSet
      (nextPoint, acc ++ newPoints)
    }
    allNewPoints
  }

  def addSand(insertPoint: Point2D, input: InputDay14): (InputDay14, Option[Point2D]) = {
    val maybeNewPoint = doFindNextPoint(insertPoint, input)
    val newState      = maybeNewPoint.map(input.addSand).getOrElse(input)
    (newState, maybeNewPoint)
  }

  @tailrec
  private def doFindNextPoint(currentPoint: Point2D, input: InputDay14): Option[Point2D] =
    if (input.isOutside(currentPoint))
      None
    else if (!input.contains(currentPoint.plusY(1)))
      doFindNextPoint(currentPoint.plusY(1), input)
    else if (input.contains(currentPoint.minusX(1).plusY(1)) && input.contains(currentPoint.plusX(1).plusY(1)))
      Some(currentPoint)
    else if (input.contains(currentPoint.minusX(1).plusY(1)))
      doFindNextPoint(currentPoint.plusX(1).plusY(1), input)
    else
      doFindNextPoint(currentPoint.minusX(1).plusY(1), input)

  def countSand(insertPoint: Point2D, input: InputDay14): Int =
    doCount(insertPoint, input, 0)

  @tailrec
  private def doCount(insertPoint: Point2D, input: InputDay14, currentCount: Int): Int = {
    val (newState, _) = addSand(insertPoint, input)
    if (newState.points == input.points)
      currentCount
    else
      doCount(insertPoint, newState, currentCount + 1)
  }

  def countSandV2(insertPoint: Point2D, input: InputDay14): Int =
    doCountV2(insertPoint, input, 0)

  @tailrec
  private def doCountV2(insertPoint: Point2D, input: InputDay14, currentCount: Int): Int = {
    val (newState, maybeNewPoint) = addSand(insertPoint, input)
    if (maybeNewPoint.contains(insertPoint))
      currentCount + 1
    else
      doCountV2(insertPoint, newState, currentCount + 1)
  }
}
