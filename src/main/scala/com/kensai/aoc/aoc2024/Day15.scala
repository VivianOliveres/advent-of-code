package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec

object Day15 {
  // TODO: refactorize to reuse Part1 & Part2

  sealed trait Direction
  case object Up    extends Direction
  case object Down  extends Direction
  case object Left  extends Direction
  case object Right extends Direction

  case class Day15Input(walls: Set[Point2D], robot: Point2D, boxes: Set[Point2D], remainingMovements: List[Direction])

  case class BigPoint(left: Point2D, right: Point2D) {

    def computeGps(): Int =
      left.y * 100 + left.x

    def next(dir: Direction): (Point2D, Point2D) =
      dir match {
        case Up =>
          (left.minusY(1), right.minusY(1))
        case Down =>
          (left.plusY(1), right.plusY(1))
        case Left =>
          (left.minusX(1), right.minusX(1))
        case Right =>
          (left.plusX(1), right.plusX(1))
      }
  }
  case class Day15InputPart2(walls: Set[BigPoint], robot: Point2D, boxes: Set[BigPoint], remainingMovements: List[Direction])

  def parse(rawRows: String): Day15Input = {
    val parts = rawRows.split("\n\n")

    // Grid
    val grid = parts.head.split("\n").toSeq
    val maxX = grid.head.length - 1
    val maxY = grid.size - 1
    val (robot, boxes, walls) = (for {
      y <- 0 to maxY
      x <- 0 to maxX
    } yield grid(y)(x) match {
      case 'O' =>
        (Option.empty[Point2D], Option(Point2D(x, y)), Option.empty[Point2D])
      case '@' =>
        (Option(Point2D(x, y)), Option.empty[Point2D], Option.empty[Point2D])
      case '#' =>
        (Option.empty[Point2D], Option.empty[Point2D], Option(Point2D(x, y)))
      case _ => (Option.empty[Point2D], Option.empty[Point2D], Option.empty[Point2D])
    }).foldLeft((Option.empty[Point2D], Set.empty[Point2D], Set.empty[Point2D])) {
      case ((maybeRobotAcc, boxesAcc, wallsAcc), (maybeRobot, boxes, walls)) =>
        (maybeRobot, boxes, walls) match {
          case (Some(_), _, _) =>
            (maybeRobot, boxesAcc, wallsAcc)
          case (_, boxes, _) if boxes.nonEmpty =>
            (maybeRobotAcc, boxesAcc ++ boxes, wallsAcc)
          case (_, _, walls) if walls.nonEmpty =>
            (maybeRobotAcc, boxesAcc, wallsAcc ++ walls)
          case _ =>
            (maybeRobotAcc, boxesAcc, wallsAcc)
        }
    }

    // Movements
    val movementsStr = parts(1).split("\n").toSeq
    val movements = movementsStr
      .flatMap(str =>
        str
          .map {
            case '^' => Up
            case '<' => Left
            case '>' => Right
            case 'v' => Down
            case _ =>
              throw new IllegalArgumentException("Should not happen")
          }
          .map(s => s.asInstanceOf[Direction])
      )
      .toList

    // Input
    Day15Input(walls, robot.get, boxes, movements)
  }

  def doMoveOnce(input: Day15Input, dir: Direction, robot: Point2D, boxes: Set[Point2D]): (Point2D, Set[Point2D]) = {
    val next = nextPos(robot, dir)
    if (input.walls.contains(next))
      (robot, boxes)
    else if (boxes.contains(next)) {
      val (newRobot, maybeNewBox) = resolveBox(dir, robot, boxes, input.walls)
      val newBoxes                = maybeNewBox.map(b => boxes - newRobot + b).getOrElse(boxes)
      (newRobot, newBoxes)
    } else {
      (next, boxes)
    }
  }

  private def nextPos(current: Point2D, direction: Direction): Point2D = direction match {
    case Up    => current.minusY(1)
    case Down  => current.plusY(1)
    case Left  => current.minusX(1)
    case Right => current.plusX(1)
  }

  private def resolveBox(direction: Direction, robot: Point2D, boxes: Set[Point2D], walls: Set[Point2D]): (Point2D, Option[Point2D]) =
    direction match {
      case Up =>
        val nextPoints = (robot.y - 1 to 0 by -1).map(y => robot.copy(y = y))
        doResolveBox(robot, nextPoints, boxes, walls)
      case Down =>
        val maxY       = walls.map(_.y).max
        val nextPoints = (robot.y + 1 to maxY).map(y => robot.copy(y = y))
        doResolveBox(robot, nextPoints, boxes, walls)
      case Left =>
        val nextPoints = (robot.x - 1 to 0 by -1).map(x => robot.copy(x = x))
        doResolveBox(robot, nextPoints, boxes, walls)
      case Right =>
        val maxX       = walls.map(_.x).max
        val nextPoints = (robot.x + 1 to maxX).map(x => robot.copy(x = x))
        doResolveBox(robot, nextPoints, boxes, walls)
    }

  private def doResolveBox(robot: Point2D, nextPoints: Seq[Point2D], boxes: Set[Point2D], walls: Set[Point2D])
      : (Point2D, Option[Point2D]) = {
    val maybeNext = nextPoints
      .find(p => !boxes.contains(p) && !walls.contains(p) || walls.contains(p))
      .map { p =>
        if (!boxes.contains(p) && !walls.contains(p))
          (nextPoints.head, Option(p))
        else
          (robot, Option.empty[Point2D])
      }
    maybeNext.get
  }

  @tailrec
  def doMove(input: Day15Input, remaining: List[Direction], robot: Point2D, boxes: Set[Point2D]): Day15Input = remaining match {
    case head :: xs =>
      val (newRobot, newBoxes) = doMoveOnce(input, head, robot, boxes)
      doMove(input, xs, newRobot, newBoxes)
    case Nil =>
      input.copy(robot = robot, boxes = boxes)
  }

  def moveAll(input: Day15.Day15Input): Day15Input =
    doMove(input, input.remainingMovements, input.robot, input.boxes)

  def sumBoxesCoordinates(input: Day15Input): Int = {
    val result = moveAll(input)
    result.boxes.map(b => b.y * 100 + b.x).sum
  }

  def extend(input: Day15Input): Day15InputPart2 = {
    def x2(p: Point2D) = p.copy(x = p.x * 2)
    val newRobot       = x2(input.robot)
    val newWalls       = input.walls.map(p => BigPoint(x2(p), x2(p).plusX(1)))
    val newBoxes       = input.boxes.map(p => BigPoint(x2(p), x2(p).plusX(1)))
    Day15InputPart2(newWalls, newRobot, newBoxes, input.remainingMovements)
  }

  def sumBoxesCoordinates2(input: Day15InputPart2): Int = {
    val result = moveAll2(input)
    result.boxes.map(_.computeGps()).sum
  }

  @tailrec
  def doMove2(input: Day15InputPart2, remaining: List[Direction], robot: Point2D, boxes: Set[BigPoint]): Day15InputPart2 = remaining match {
    case head :: xs =>
      val (newRobot, toRemoveBoxes, newBoxes) = doMoveOnce2(input, head, robot, boxes)
      doMove2(input, xs, newRobot, boxes -- toRemoveBoxes ++ newBoxes)
    case Nil =>
      input.copy(robot = robot, boxes = boxes)
  }

  def moveAll2(input: Day15.Day15InputPart2): Day15InputPart2 =
    doMove2(input, input.remainingMovements, input.robot, input.boxes)

  def doMoveOnce2(input: Day15InputPart2, dir: Direction, robot: Point2D, boxes: Set[BigPoint]): (Point2D, Set[BigPoint], Set[BigPoint]) =
    if (isEmpty(robot, dir, boxes, input.walls))
      (nextPos(robot, dir), Set(), boxes)
    else if (hitWall(robot, dir, boxes, input.walls))
      (robot, Set(), boxes)
    else {
      // Move all other BigPoint in the direction
      val allBoxes    = nextBoxesFrom(robot, dir, boxes, Set())
      val nextRobot   = nextPos(robot, dir)
      val allNewBoxes = allBoxes.map(_.next(dir)).map(pair => BigPoint(pair._1, pair._2))
      (nextRobot, allBoxes, allNewBoxes)
    }

  private def isEmpty(current: Point2D, dir: Direction, boxes: Set[BigPoint], walls: Set[BigPoint]): Boolean = {
    val next      = nextPos(current, dir)
    val nextBoxes = boxes.filter(p => p.left == next || p.right == next)
    val nextWalls = walls.filter(p => p.left == next || p.right == next)
    nextBoxes.isEmpty && nextWalls.isEmpty
  }

  private def hitWall(current: Point2D, dir: Direction, boxes: Set[BigPoint], walls: Set[BigPoint]): Boolean = {
    val next    = nextPos(current, dir)
    val hasWall = walls.exists(p => p.left == next || p.right == next)
    if (hasWall)
      true
    else {
      val nextBoxes = boxes.filter(p => p.left == next || p.right == next)
      if (nextBoxes.isEmpty)
        false
      else if (dir == Right)
        hitWall(nextBoxes.head.right, dir, boxes, walls)
      else if (dir == Left)
        hitWall(nextBoxes.head.left, dir, boxes, walls)
      else {
        nextBoxes.forall(p => hitWall(p.left, dir, boxes, walls) || hitWall(p.right, dir, boxes, walls))
      }
    }
  }

  def nextBoxesFrom(current: Point2D, dir: Direction, boxes: Set[BigPoint], acc: Set[BigPoint]): Set[BigPoint] = {
    val next      = nextPos(current, dir)
    val nextBoxes = boxes.filter(p => p.left == next || p.right == next)
    if (nextBoxes.isEmpty)
      acc
    else {
      nextBoxes.foldLeft(acc) { case (boxAcc, box) =>
        val newAcc = boxAcc + box
        if (dir == Left)
          boxAcc ++ nextBoxesFrom(box.left, dir, boxes, newAcc)
        else if (dir == Right)
          boxAcc ++ nextBoxesFrom(box.right, dir, boxes, newAcc)
        else {
          val leftResult  = nextBoxesFrom(box.left, dir, boxes, newAcc)
          val rightResult = nextBoxesFrom(box.right, dir, boxes, newAcc)
          boxAcc ++ leftResult ++ rightResult
        }
      }
    }

  }

}
