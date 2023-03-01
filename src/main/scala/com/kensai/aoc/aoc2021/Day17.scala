package com.kensai.aoc.aoc2021

import scala.annotation.tailrec
import scala.collection.Seq

object Day17 {

  case class Inputs(minX: Int, maxX: Int, minY: Int, maxY: Int, targets: Seq[(Int, Int)])

  private val rowRegex = """target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)""".r
  private def parse(input: String): Inputs = input match {
    case rowRegex(minXStr, maxXStr, minYStr, maxYStr) =>
      val (minX, maxX, minY, maxY) = (minXStr.toInt, maxXStr.toInt, minYStr.toInt, maxYStr.toInt)
      val targets = for {
        x <- minX to maxX
        y <- minY to maxY
      } yield (x, y)
      Inputs(minX, maxX, minY, maxY, targets)
    case _ => throw new IllegalArgumentException(s"Invalid str[$input]")
  }

  /** Generate all the initial velocities that will goes threw the target square. But return only the one that goes to the highest Y
    * coordinates
    * @return
    *   ((xInitVelocity, yInitVelocity), maxYReached)
    */
  def computeHighestVelocityY(input: String): ((Int, Int), Int) = {
    val inputs  = parse(input)
    val results = generateValidVelocity(inputs)
    results.maxBy(_._2)
  }

  /** Generate all possible (and realistic) velocities. Then filter only the ones that goes threw the target square.
    * @return
    *   ((xInitVelocity, yInitVelocity), maxYReached)
    */
  private def generateValidVelocity(inputs: Inputs): Seq[((Int, Int), Int)] =
    for {
      initVelocityX <- 0 to inputs.maxX
      initVelocityY <- inputs.minY to math.abs(inputs.minY)
      validSolution <- findValidVelocity((initVelocityX, initVelocityY), (initVelocityX, initVelocityY), (0, 0), 0, inputs)
    } yield validSolution

  /** Check if this initialVelocity, that is now at currentPos with currentVelocity will ends in the target square. If not, return None.
    */
  @tailrec
  private def findValidVelocity(
      initVelocity: (Int, Int),
      currentVelocity: (Int, Int),
      currentPos: (Int, Int),
      maxYReached: Int,
      inputs: Inputs
    ): Option[((Int, Int), Int)] =
    if (currentPos._2 < inputs.minY) // Too far away on the y axis
      None
    else if (currentPos._1 < inputs.minX && currentVelocity._1 == 0) // Too far away on the x axis (on the left side)
      None
    else if (currentPos._1 > inputs.maxX) // Too far away on the x axis (on the right side)
      None
    else if (inputs.targets.contains(currentPos)) // Find a valid solution
      Some((initVelocity, maxYReached))
    else {
      // Let's continue the journey
      val (newX, newXVelocity) =
        if (currentVelocity._1 == 0) (currentPos._1, 0) else (currentPos._1 + currentVelocity._1, currentVelocity._1 - 1)
      val (newY, newYVelocity) = (currentPos._2 + currentVelocity._2, currentVelocity._2 - 1)
      val newMaxYReached       = math.max(newY, maxYReached)
      findValidVelocity(initVelocity, (newXVelocity, newYVelocity), (newX, newY), newMaxYReached, inputs)
    }

  /** Count the number of different initial velocity that will end in the target square.
    */
  def countVelocityY(input: String): Int = {
    val inputs  = parse(input)
    val results = generateValidVelocity(inputs)
    results.size
  }

}
