package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec


object Day16 {

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  case class Day16Input(walls: Set[Point2D], startDirection: Direction, start: Point2D, end: Point2D)

  def parse(rows: Seq[String]): Day16Input = {
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
        throw new IllegalArgumentException(s"Should not happen: [$current]")
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
    Day16Input(walls, East, maybeS.get, maybeE.get)
  }

  def bestScore(input: Day16Input): Int = {
    val (bestScore, _) = move(input, Map(), List((PointDir(input.start, input.startDirection), 0, Seq(input.start))), Int.MaxValue, Seq())
    bestScore
  }

  def bestSeats(input: Day16Input): Int = {
    val (_, seats) = move(input, Map(), List((PointDir(input.start, input.startDirection), 0, Seq(input.start))), Int.MaxValue, Seq())
    seats.flatMap(s => s.toSet).toSet.size
  }

  case class PointDir(p: Point2D, dir: Direction)

  @tailrec
  private def move(input: Day16Input, seen: Map[PointDir, Int], todo: List[(PointDir, Int, Seq[Point2D])], bestPath: Int, possibleBestPath: Seq[Seq[Point2D]]): (Int, Seq[Seq[Point2D]]) = todo match {
    case (current, currentCount, currentPath) :: xs =>
      if (current.p == input.end){
        if (currentCount < bestPath)
          move(input, seen, xs, currentCount, Seq(currentPath :+ input.end))
        else if (currentCount == bestPath)
          move(input, seen, xs, currentCount, possibleBestPath :+ (currentPath :+ input.end))
        else
          move(input, seen, xs, bestPath, possibleBestPath)
      } else if (currentCount > bestPath){
        // Too long => skip it
        move(input, seen, xs, bestPath, possibleBestPath)
      } else if (seen.contains(current) && seen(current) < currentCount) {
        // Already find a better path to this point => do not continue
        move(input, seen, xs, bestPath, possibleBestPath)
      } else {
        val newSeen = seen + (current -> currentCount)
        val nextMoves = computeNextMoves(current, currentCount, currentPath, input.walls)
        val newTodo = (xs ++ nextMoves)//.sortBy(_._2) // Keep smallest count first
        move(input, newSeen, newTodo, bestPath, possibleBestPath)
      }
    case Nil =>
      (bestPath, possibleBestPath)
  }

  private def computeNextMoves(current: PointDir, currentCount: Int, currentPath: Seq[Point2D], walls: Set[Point2D]) : List[(PointDir, Int, Seq[Point2D])] = {
    val right = Point2D(current.p.x + 1, current.p.y)
    val countRight = current.dir match {
      case East => currentCount + 1
      case _ => currentCount + 1001
    }

    val left = Point2D(current.p.x - 1, current.p.y)
    val countLeft = current.dir match {
      case West => currentCount + 1
      case _ => currentCount + 1001
    }

    val bottom = Point2D(current.p.x, current.p.y + 1)
    val countBottom = current.dir match {
      case South => currentCount + 1
      case _ => currentCount + 1001
    }

    val top = Point2D(current.p.x, current.p.y - 1)
    val countTop = current.dir match {
      case North => currentCount + 1
      case _ => currentCount + 1001
    }

    // No coming back
    val allPoints = current.dir match {
      case North => Seq((PointDir(top, North), countTop), (PointDir(left, West), countLeft), (PointDir(right, East), countRight))
      case South => Seq((PointDir(bottom, South), countBottom), (PointDir(left, West), countLeft), (PointDir(right, East), countRight))
      case East => Seq((PointDir(top, North), countTop), (PointDir(right, East), countRight), (PointDir(bottom, South), countBottom))
      case West => Seq((PointDir(top, North), countTop), (PointDir(left, West), countLeft), (PointDir(bottom, South), countBottom))
    }

    val allPointsFiltered = allPoints
      .filterNot(p => walls.contains(p._1.p))
      .map{case (p, c) => (p, c, currentPath :+ p.p)}

    allPointsFiltered.toList
  }


}
