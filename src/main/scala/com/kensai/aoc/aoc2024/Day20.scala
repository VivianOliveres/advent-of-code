package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

import scala.collection.parallel.CollectionConverters._

object Day20 {

  case class Day20Input(maxX: Int, maxY: Int, walls: Set[Point2D], start: Point2D, end: Point2D)

  def parse(rows: Seq[String]): Day20Input = {
    val maxX = rows.head.length - 1
    val maxY = rows.size - 1
    val allPoints: Seq[(Option[Point2D], Option[Point2D], Option[Point2D])] = for {
      y <- 0 to maxY
      x <- 0 to maxX
    } yield {
      val current = rows(y)(x)
      if (current == '.')
        (None, None, None)
      else if (current == 'S')
        (Option(Point2D(x, y)), None, None)
      else if (current == 'E')
        (None, Option(Point2D(x, y)), None)
      else if (current == '#')
        (None, None, Option(Point2D(x, y)))
      else
        throw new IllegalArgumentException(s"Should not happen : [$x, $y] [$current]")
    }

    val (maybeS, maybeE, walls) = allPoints.foldLeft((Option.empty[Point2D], Option.empty[Point2D], Set.empty[Point2D])){case ((maybeSAcc, maybeEAcc, maybeWallsAcc), (maybeS, maybeE, maybeWalls)) =>
      (maybeS, maybeE, maybeWalls) match {
        case (Some(_), _, _) => (maybeS, maybeEAcc, maybeWallsAcc)
        case (_, Some(_), _) => (maybeSAcc, maybeE, maybeWallsAcc)
        case (_, _, Some(wall)) => (maybeSAcc, maybeEAcc, maybeWallsAcc + wall)
        case (None, None, None) => (maybeSAcc, maybeEAcc, maybeWallsAcc)
        case acc => throw new IllegalArgumentException(s"Should not happen: $acc")
      }
    }

    Day20Input(maxX, maxY, walls, maybeS.get, maybeE.get)
  }

  def move(maxX: Int, maxY: Int, walls: Set[Point2D], end: Point2D, todo: Seq[(Point2D, Seq[Point2D])], memo: Map[Point2D, Int]): Seq[Point2D] = {
    if (todo.isEmpty)
      throw new IllegalArgumentException(s"Should not happen")
    else {
     val (current, currentPath) = todo.head
      if (current == end) {
       currentPath
     } else if (memo.contains(current) && currentPath.size >= memo(current)) {
       move(maxX, maxY, walls, end, todo.tail, memo)
     } else {
       val nextMoves = current.nextPoints()
         .filter(_.in(maxX, maxY))
         .filterNot(walls.contains)
         .filterNot(memo.contains)
         .map(p => (p, currentPath :+ p))
       val nextMemo = memo + (current -> currentPath.size)
       val newTodo = (todo.tail ++ nextMoves).sortBy(_._2.size)
       move(maxX, maxY, walls, end, newTodo, nextMemo)
     }
    }
  }

  def initialPath(input: Day20Input): Seq[Point2D] = {
    val todo = Seq((input.start, Seq()))
    move(input.maxX, input.maxY, input.walls, input.end, todo, Map())
  }

  private def pointsAt(pos: Point2D, maxX: Int, maxY: Int, dist: Int): Seq[Point2D] =
    for {
      y <- -pos.y - dist to pos.y + dist
      x <- -pos.x - dist to pos.x + dist
      other = Point2D(x, y)
      if pos != other
      if pos.manhattanDistance(other) <= dist
      if other.in(maxX, maxY)
    } yield other

  def computeCheating(input: Day20Input, pico: Int): Int = {
    val initPath = initialPath(input)
    val initFullPath = input.start +: initPath

    // TODO: parallel collections to be faster (should find a better solution
    val pathWithIndex = initFullPath.zipWithIndex.par
    val shortcutsPar = pathWithIndex.flatMap{case (from, fromIndex) => pointsAt(from, input.maxX, input.maxY, pico).map {to =>
      (from, fromIndex, to, initFullPath.indexOf(to))
    }}

    val results = shortcutsPar
      .filter{case (_, _, to, _) => initFullPath.contains(to)}
      .filter{case (_, fromIndex, _, toIndex) => toIndex >= fromIndex + 100}
      .map{case (from, fromIndex, to, toIndex) => initFullPath.size - (toIndex - fromIndex) + from.manhattanDistance(to)}
      .filter{newPathSize => newPathSize <= initFullPath.size - 100}

    results.size
  }

  def countUnder100Cheating(input: Day20Input, pico: Int): Int = {
    val allCheating = computeCheating(input, pico)
    allCheating
  }

}
