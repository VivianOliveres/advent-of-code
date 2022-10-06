package com.kensai.aoc.aoc2021

object Day20 {

  case class Point(x: Int, y: Int)
  case class Image(points: Map[Point, Boolean], outerPointsValue: Boolean)
  case class Day20Input(enhancementAlgorithm: Seq[Boolean], image: Image)

  def parse(rows: Seq[String]): Day20Input = {
    val enhancementAlgorithm = rows.head.map(_ == '#')
    val image = (2 until rows.size).flatMap{case rowIndex =>
      val row = rows(rowIndex)
      row.zipWithIndex.map{case (char, columnIndex) =>
        val value = char == '#'
        Point(columnIndex, rowIndex - 2) -> value
      }
    }.toMap
    Day20Input(enhancementAlgorithm, Image(image, false))
  }

  private def binToDecimal(values: Seq[Boolean]): Int =
    values.reverse.zipWithIndex.foldLeft(0){case (acc, (bool, index)) =>
      val value = if (bool) 1 else 0
      acc + value * math.pow(2.0, index.toDouble).toInt
    }

  private def minMax(points: Seq[Point]): (Int, Int, Int, Int) =
    points.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)) { case ((minX, maxX, minY, maxY), point) =>
      (math.min(minX, point.x), math.max(maxX, point.x), math.min(minY, point.y), math.max(maxY, point.y))
    }

  def printImage(image: Map[Point, Boolean]): Unit = {
    val (minX, maxX, minY, maxY) = minMax(image.keys.toSeq)
    for {
      y <- minY to maxY
      x <- minX to maxX
    } yield {
      val bool = image.getOrElse(Point(x, y), false)
      val value = if (bool) "#" else "."
      print(value)
      if (x == maxX) println()
      ()
    }
    ()
  }

  def countPixelsLit(input: Day20Input, steps: Int): Int = {
    val result = (0 until steps).foldLeft(input.image) {case (image, _) =>
      val paddedPoints = applyPadding(image)
      val points = applyImageEnhancer(input.enhancementAlgorithm, paddedPoints, image.outerPointsValue)
      val newOuterPointsValue = if(image.outerPointsValue) input.enhancementAlgorithm.last else input.enhancementAlgorithm.head
      Image(points, newOuterPointsValue)
    }

    printImage(result.points)
    result.points.values.count(_ == true)
  }

  /**
    * Create all points at paddingValue distance from other points and initialize them to image.outerPointsValue
    */
  private def applyPadding(image: Image, paddingValue: Int = 1): Map[Point, Boolean] = {
    val points = image.points
    val (minX, maxX, minY, maxY) = minMax(points.keys.toSeq)
    val paddingPoints = (for {
      x <- minX - paddingValue to maxX + paddingValue
      y <- minY - paddingValue to maxY + paddingValue
      point = Point(x, y) if !points.contains(point)
    } yield {
      point -> image.outerPointsValue
    }).toMap

    points ++ paddingPoints
  }

  private def applyImageEnhancer(enhancementAlgorithm: Seq[Boolean], points: Map[Point, Boolean], outerPointsValue: Boolean): Map[Point, Boolean] =
    points.foldLeft(Map.empty[Point, Boolean]) { case (acc, (point, _)) =>
      val neighbourValues = for {
        y <- point.y - 1 to point.y + 1
        x <- point.x - 1 to point.x + 1
        otherPoint = Point(x, y)
      } yield points.getOrElse(otherPoint, outerPointsValue)

      val enhancerIndex = binToDecimal(neighbourValues)
      val newLit = enhancementAlgorithm(enhancerIndex)
      acc + (point -> newLit)
    }

}
