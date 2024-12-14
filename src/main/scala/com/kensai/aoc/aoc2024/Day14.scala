package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

object Day14 {

  case class Robot(pos: Point2D, velocity: Point2D)
  case class Day14Input(maxX: Int, maxY: Int, robots: Seq[Robot])

  private val rowRegex = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r

  def parse(wide: Int, tall: Int, rows: Seq[String]): Day14Input = {
    val robots = rows
      .map {
        case rowRegex(px, py, vx, vy) =>
          Robot(Point2D(px.toInt, py.toInt), Point2D(vx.toInt, vy.toInt))
        case row =>
          throw new IllegalArgumentException(s"Parse failed: [$row]")
      }
      .sortBy(r => (r.pos.y, r.pos.x))
    Day14Input(wide - 1, tall - 1, robots)
  }

  private def move(max: Int, r: Int, v: Int, times: Int): Int = {
    val mul = r + v * times
    val mod = max + 1
    // Manage negative with modulo
    (mul % mod + mod) % mod
  }

  def move(input: Day14Input, robot: Robot, times: Int): Robot = {
    val x = move(input.maxX, robot.pos.x, robot.velocity.x, times)
    val y = move(input.maxY, robot.pos.y, robot.velocity.y, times)
    robot.copy(pos = Point2D(x, y))
  }

  def move(input: Day14Input, times: Int): Day14Input = {
    val board = input.robots.map(r => move(input, r, times))
    input.copy(robots = board)
  }

  def safetyFactor(input: Day14Input, times: Int): Int = {
    val board   = move(input, times).robots
    val middleX = input.maxX / 2
    val middleY = input.maxY / 2
    val (countLowerLeft, countLowerRight, countUpperLeft, countUpperRight) = board
      .filterNot(r => r.pos.x == middleX || r.pos.y == middleY)
      .foldLeft((0, 0, 0, 0)) { case ((accLowerLeft, accLowerRight, accUpperLeft, accUpperRight), r) =>
        if (r.pos.x < middleX && r.pos.y < middleY)
          (accLowerLeft + 1, accLowerRight, accUpperLeft, accUpperRight)
        else if (r.pos.x < middleX && r.pos.y > middleY)
          (accLowerLeft, accLowerRight + 1, accUpperLeft, accUpperRight)
        else if (r.pos.x > middleX && r.pos.y < middleY)
          (accLowerLeft, accLowerRight, accUpperLeft + 1, accUpperRight)
        else // (r.pos.x > middleX && r.pos.y > middleY)
          (accLowerLeft, accLowerRight, accUpperLeft, accUpperRight + 1)
      }
    countLowerLeft * countLowerRight * countUpperLeft * countUpperRight
  }

  def printUniqueBoard(input: Day14Input, max: Int = 10000): Option[Int] =
    (0 to max).find { i =>
      val result      = move(input, i)
      val robotCounts = result.robots.map(_.pos).groupMapReduce(identity)(_ => 1)(_ + _)
      val isUnique    = robotCounts.forall(_._2 <= 1)
      if (isUnique) {
        println(s"-------------------------------")
        println(s"------------- $i ---------------")
        printGrid(result)
      }
      isUnique
    }

  def printGrid(input: Day14Input): Unit = {
    val robotCounts = input.robots.map(_.pos).groupMapReduce(identity)(_ => 1)(_ + _)
    for {
      y <- 0 to input.maxY
      x <- 0 to input.maxX
    } yield {
      val current = Point2D(x, y)
      val str = robotCounts
        .get(current)
        .map(_.toString)
        .getOrElse(".")
      print(str)
      if (x == input.maxX)
        println("")
    }
    ()
  }

}
