package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

object Day10 {

  case class Day10Input(grid: Map[Point2D, Int], trailHeads: Seq[Point2D], maxX: Int, maxY: Int)

  def parse(rows: Seq[String]): Day10Input = {
    val maxX = rows.head.length - 1
    val maxY = rows.size - 1
    val grid = (for {
      x <- 0 to maxX
      y <- 0 to maxY
    } yield (Point2D(x, y), rows(y)(x).toString.toInt)).toMap
    val trailHeads = grid.filter(_._2 == 0).keys.toSeq
    Day10Input(grid = grid, trailHeads = trailHeads, maxX = maxX, maxY = maxY)
  }

  def hikingScore(input: Day10Input): Int =
    input.trailHeads
      .map(head => doRate(input = input, currentPos = head, acc = Seq()).toSet.size)
      .sum

  def hikingScore(input: Day10Input, trailHead: Point2D): Int =
    doRate(input = input, currentPos = trailHead, acc = Seq()).toSet.size

  def hikingRating(input: Day10Input): Int =
    input.trailHeads.map(head => hikingRating(input, head)).sum

  def hikingRating(input: Day10Input, trailHead: Point2D): Int =
    doRate(input = input, currentPos = trailHead, acc = Seq()).size

  private def doRate(input: Day10Input, currentPos: Point2D, acc: Seq[Point2D]): Seq[Point2D] = {
    val currentValue = input.grid(currentPos)
    if (currentValue == 9)
      acc :+ currentPos
    else {
      val nextPos = Seq(
        currentPos.plusX(1),
        currentPos.plusX(-1),
        currentPos.plusY(1),
        currentPos.plusY(-1)
      ).filter(pos => pos.y >= 0 && pos.x >= 0 && pos.y <= input.maxY && pos.x <= input.maxX)
        .filter(pos => input.grid(pos) == currentValue + 1)
      nextPos
        .map(pos => doRate(input, pos, acc))
        .foldLeft(Seq.empty[Point2D])(_ ++ _)
    }
  }

}
