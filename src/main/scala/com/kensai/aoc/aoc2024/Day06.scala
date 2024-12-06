package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec

import scala.collection.parallel.CollectionConverters._

object Day06 {

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East  extends Direction
  case object West  extends Direction

  case class Day6Input(maxX: Int, maxY: Int, guardPos: Point2D, guardDirection: Direction, obstacles: Set[Point2D])

  case class Day6Tmp(input: Day6Input, guardPos: Point2D, guardDirection: Direction, path: Set[Point2D])

  def parse(rows: Seq[String]): Day6Input = {
    val maxX = rows.head.length - 1
    val maxY = rows.length - 1

    @tailrec
    def doParse(x: Int, y: Int, maybeGuard: Option[Point2D], maybeGuardDirection: Option[Direction], obstacles: Set[Point2D]): Day6Input =
      if (y > maxY)
        Day6Input(maxX, maxY, maybeGuard.get, maybeGuardDirection.get, obstacles)
      else if (x > maxX)
        doParse(0, y + 1, maybeGuard, maybeGuardDirection, obstacles)
      else if (rows(y)(x) == '#')
        doParse(x + 1, y, maybeGuard, maybeGuardDirection, obstacles + Point2D(x, y))
      else if (rows(y)(x) == '^')
        doParse(x + 1, y, Some(Point2D(x, y)), Some(North), obstacles)
      else if (rows(y)(x) == '>')
        doParse(x + 1, y, Some(Point2D(x, y)), Some(East), obstacles)
      else if (rows(y)(x) == 'v')
        doParse(x + 1, y, Some(Point2D(x, y)), Some(South), obstacles)
      else if (rows(y)(x) == '<')
        doParse(x + 1, y, Some(Point2D(x, y)), Some(West), obstacles)
      else
        doParse(x + 1, y, maybeGuard, maybeGuardDirection, obstacles)

    doParse(0, 0, None, None, Set())
  }

  def countGuardPath(input: Day6Input): Int = {
    val finalPath = moveGuardUntilEnd(
      Day6Tmp(input = input, guardPos = input.guardPos, guardDirection = input.guardDirection, path = Set())
    )
    finalPath.size
  }

  def moveGuardUntilEnd(state: Day6Tmp): Set[Point2D] =
    doMoveGuardUntilEnd(state)

  @tailrec
  private def doMoveGuardUntilEnd(state: Day6Tmp): Set[Point2D] = {
    val (maybeNextState, path) = moveGuard(state)
    maybeNextState match {
      case None            => path
      case Some(nextState) => doMoveGuardUntilEnd(nextState)
    }
  }

  def moveGuard(state: Day6Tmp): (Option[Day6Tmp], Set[Point2D]) = {
    val (maybeGuard, newPath) = state.guardDirection match {
      case North =>
        val maybeObstacle = (state.guardPos.y to 0 by -1)
          .map(y => Point2D(state.guardPos.x, y))
          .find(state.input.obstacles.contains)
        val maybeNewPos   = maybeObstacle.map(p => (p.plusY(1), East))
        val newPath = state.path ++
          maybeNewPos
            .map { case (newpos, _) => state.guardPos.y to newpos.y by -1 }
            .getOrElse(state.guardPos.y to 0 by -1)
            .map(y => Point2D(state.guardPos.x, y))
        (maybeNewPos, newPath)
      case East =>
        val maybeObstacle = (state.guardPos.x to state.input.maxX)
            .map(x => Point2D(x, state.guardPos.y))
            .find(state.input.obstacles.contains)
        val maybeNewPos = maybeObstacle.map(p => (p.minusX(1), South))
        val newPath = state.path ++
          maybeNewPos
            .map { case (newpos, _) => state.guardPos.x to newpos.x }
            .getOrElse(state.guardPos.x to state.input.maxX)
            .map(x => Point2D(x, state.guardPos.y))
        (maybeNewPos, newPath)
      case South =>
        val maybeObstacle = (state.guardPos.y to state.input.maxY)
            .map(y => Point2D(state.guardPos.x, y))
            .find(state.input.obstacles.contains)
        val maybeNewPos = maybeObstacle.map(p => (p.minusY(1), West))
        val newPath = state.path ++
          maybeNewPos
            .map { case (newpos, _) => state.guardPos.y to newpos.y }
            .getOrElse(state.guardPos.y to state.input.maxY)
            .map(y => Point2D(state.guardPos.x, y))
        (maybeNewPos, newPath)
      case West =>
        val maybeObstacle = (state.guardPos.x to 0 by -1)
          .map(x => Point2D(x, state.guardPos.y))
          .find(state.input.obstacles.contains)
        val maybeNewPos   = maybeObstacle.map(p => (p.plusX(1), North))
        val newPath = state.path ++
          maybeNewPos
            .map { case (newpos, _) => state.guardPos.x to newpos.x by -1 }
            .getOrElse(state.guardPos.x to 0 by -1)
            .map(x => Point2D(x, state.guardPos.y))
        (maybeNewPos, newPath)
    }

    maybeGuard match {
      case None => // Out of the grid
        (None, newPath)
      case Some((guardPos, guardDirection)) =>
        (Some(state.copy(guardPos = guardPos, guardDirection = guardDirection, path = newPath)), newPath)
    }
  }

  // TODO: improve perf
  // It is 10s on laptop thanks to parallel collections
  def countLoops(input: Day6Input): Int = {
    // Possible points are points on the path to the guard.
    // Else it will not impact the result.
    // So we reuse solution from part1
    val possiblePoints = moveGuardUntilEnd(
      Day6Tmp(input = input, guardPos = input.guardPos, guardDirection = input.guardDirection, path = Set())
    )
    possiblePoints
      .par // Parallelize to reduce to 10s instead of 1min
      .filterNot(_ == input.guardPos)
      .count(isLoop(input, _))
  }

  private def isLoop(input: Day6Input, pos: Point2D): Boolean = {
    val tmp = Day6Tmp(
      input = input.copy(obstacles = input.obstacles + pos),
      guardPos = input.guardPos,
      guardDirection = input.guardDirection,
      path = Set()
    )
    val (isLoop, _) = doMoveGuardUntilEndOrLoop(tmp, Set((input.guardPos, input.guardDirection)))
    isLoop
  }

  @tailrec
  private def doMoveGuardUntilEndOrLoop(state: Day6Tmp, visited: Set[(Point2D, Direction)]): (Boolean, Set[Point2D]) = {
    val (maybeNextState, path) = moveGuard(state)
    val diffState              = path.diff(state.path)
    maybeNextState match {
      case None =>
        (false, path)
      case Some(nextState) if visited.contains((nextState.guardPos, nextState.guardDirection)) =>
        (true, path)
      case Some(nextState) =>
        doMoveGuardUntilEndOrLoop(nextState, visited ++ diffState.map(p => (p, nextState.guardDirection)))
    }
  }

}
