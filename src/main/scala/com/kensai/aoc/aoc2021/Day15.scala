package com.kensai.aoc.aoc2021

import scala.annotation.tailrec

object Day15 {

  case class CaveCell(x: Int, y: Int, risk: Int) {
    def pos: (Int, Int) = (x, y)
  }

  case class Cave(positions: Map[(Int, Int), CaveCell]) {
    val maxX: Int = positions.keys.map(_._1).max
    val maxY: Int = positions.keys.map(_._2).max

    def apply(pos: (Int, Int)): CaveCell =
      positions(pos)

    def apply(x: Int, y: Int): CaveCell =
      positions((x, y))

    def risk(pos: (Int, Int)): Int =
      positions(pos).risk

    /** The exit cell.
      */
    def exit: CaveCell =
      apply((maxX, maxY))

    /** Generate the 4 neighbours cells and filter the ones that are not in the grid.
      */
    def neighbours(pos: CaveCell): Seq[CaveCell] =
      neighbours(pos.pos)

    /** Generate the 4 neighbours cells and filter the ones that are not in the grid.
      */
    def neighbours(pos: (Int, Int)): Seq[CaveCell] =
      for {
        pos <- Seq(
          (pos._1 - 1, pos._2),
          (pos._1 + 1, pos._2),
          (pos._1, pos._2 - 1),
          (pos._1, pos._2 + 1)
        )
        cavePos <- positions.get(pos)
      } yield cavePos
  }

  def parse(lines: Seq[String]): Cave = {
    val cases = for {
      (str, y) <- lines.zipWithIndex
      (value, x) <- str.zipWithIndex
    } yield (x, y) -> CaveCell(x, y, value.asDigit)
    Cave(cases.toMap)
  }

  /** Parse input to generate a Cave but with:<br>
    * <ul>
    *     <li>5 more cells</li>
    *     <li>new cells have a risk that is +1 from the previous cell</li>
    *     <li>new cells cannot have a risk greater than 9 (10 will be 1)</li>
    * </ul>
    */
  def parseBiggerMap(lines: Seq[String]): Cave = {
    val initCave = parse(lines)
    val cases = initCave.positions.toSeq

    val generatedCases = for {
      x <- 0 to 4
      y <- 0 to 4
      if !(x == 0 && y == 0)
      (pos, cavePos) <- cases
      newRisk = (cavePos.risk + x + y - 1) % 9 + 1
    } yield (
      (pos._1 + x * (initCave.maxX + 1), pos._2 + y * (initCave.maxY + 1)),
      CaveCell(
        pos._1 + x * (initCave.maxX + 1),
        pos._2 + y * (initCave.maxY + 1),
        newRisk
      )
    )

    val mapCases = (cases ++ generatedCases).toMap
    Cave(mapCases)
  }

  /** Compute the best path from entry to every cells. It is a Dijkstra computation.
    * @param cave: The input cave.
    * @param riskCache: All cells already computed with the sum of risks to reach each of them.
    * @param nextPositions: Next positions to compute. Should be sorted from minRisk to maxRisk.
    * @return riskCache for every cells in the Cave.
    */
  @tailrec
  private def doComputeLowestRiskPath(
      cave: Cave,
      riskCache: Map[(Int, Int), Int],
      nextPositions: Seq[(CaveCell, Int)]
  ): Map[(Int, Int), Int] = {
    if (nextPositions.isEmpty)
      riskCache
    else {
      val (currentCell, currentRisk) = nextPositions.head
      val updatedCache = riskCache + (currentCell.pos -> currentRisk)
      val updatedNextPositions =
        updateNextPositions(cave, currentCell, updatedCache, nextPositions)
      doComputeLowestRiskPath(
        cave,
        updatedCache,
        updatedNextPositions
      )
    }
  }

  private def updateNextPositions(
      cave: Cave,
      currentCell: CaveCell,
      cache: Map[(Int, Int), Int],
      nextPositions: Seq[(CaveCell, Int)]
  ): Seq[(CaveCell, Int)] = {
    val neighboursToCompute = cave
      .neighbours(currentCell)
      .filterNot(tmp =>
        cache.contains(tmp.pos)
      ) //neighbours from currentCell that are not computed
      .flatMap { p => // Compute their minRisk
        cave
          .neighbours(p)
          .filter(n => cache.contains(n.pos))
          .minByOption(n => cache(n.pos))
          .map(n => (p, n))
      }
      .map { case (c, previous) =>
        (c, c.risk + cache(previous.pos))
      }

    // Set for performance
    val neighboursToComputeSet = neighboursToCompute.map(_._1).toSet

    // Next positions without currentCell and without neighboursToCompute (their minRisk could have changed)
    val nextPositionsUpdated =
      nextPositions.tail.filterNot(p => neighboursToComputeSet.contains(p._1))

    // Sort them manually
    (nextPositionsUpdated ++ neighboursToCompute).sortBy(_._2)
  }

  /** Compute the best path from entry to exit (ie from (0, 0) to (maxX, maxY))
    * and return the sum of risks from the visited cells.
    */
  def computeLowestRiskPath(cave: Cave): Int = {
    val riskCache = Map((cave(0, 0).pos, 0))
    val nextPositions =
      cave.neighbours(cave(0, 0)).map(p => (p, p.risk)).sortBy(_._2)
    val finalCache =
      doComputeLowestRiskPath(
        cave,
        riskCache,
        nextPositions
      )
    finalCache(cave.exit.pos)
  }

}
