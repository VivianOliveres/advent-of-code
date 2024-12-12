package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec

object Day12 {

  case class Day12Input(maxX: Int, maxY: Int, gardenPlots: Map[Point2D, Char])

  case class Region(id: Char, area: Int, perimeter: Int) {
    def add(area: Int, perimeter: Int): Region =
      this.copy(area = this.area + area, perimeter = this.perimeter + perimeter)
  }

  case class Region2(id: Char, area: Int, countCorners: Int)

  def parse(rows: Seq[String]): Day12Input = {
    val maxX = rows.head.length - 1
    val maxY = rows.size - 1
    val gardenPlots = for {
      x <- 0 to maxX
      y <- 0 to maxY
    } yield Point2D(x, y) -> rows(y)(x)
    Day12Input(maxX, maxY, gardenPlots.toMap)
  }

  def computeRegions(input: Day12Input): Seq[Region] = {
    val (regions, _) = input.gardenPlots.keys.foldLeft((Seq.empty[Region], Set.empty[Point2D])) {
      case ((regionAcc, visitedAcc), currentPos) =>
        if (visitedAcc.contains(currentPos))
          (regionAcc, visitedAcc)
        else {
          val (newRegion, newVisited) = doCompute(input, currentPos, visitedAcc, None)
          (regionAcc :+ newRegion, visitedAcc ++ newVisited)
        }
    }
    regions
  }

  def doCompute(input: Day12Input, currentPos: Point2D, visited: Set[Point2D], maybeRegion: Option[Region]): (Region, Set[Point2D]) =
    if (maybeRegion.isEmpty) {
      // Initial step
      val next          = nextPositions(input, currentPos)
      val nextUnvisited = next.filterNot(visited.contains)
      val perimeter     = 4 - next.size
      val region        = Region(input.gardenPlots(currentPos), 1, perimeter)
      nextUnvisited.foldLeft((region, Set(currentPos))) { case ((newRegionAcc, visitedAcc), nextPos) =>
        doCompute(input = input, currentPos = nextPos, visited = visitedAcc, maybeRegion = Some(newRegionAcc))
      }
    } else if (visited.contains(currentPos)) {
      (maybeRegion.get, visited)
    } else {
      val next          = nextPositions(input, currentPos)
      val nextUnvisited = next.filterNot(visited.contains)
      val perimeter     = 4 - next.size
      val region        = maybeRegion.get.add(1, perimeter)
      nextUnvisited.foldLeft((region, visited + currentPos)) { case ((newRegionAcc, visitedAcc), nextPos) =>
        doCompute(input = input, currentPos = nextPos, visited = visitedAcc, maybeRegion = Some(newRegionAcc))
      }
    }

  private def nextPositions(input: Day12Input, currentPos: Point2D): Seq[Point2D] =
    currentPos
      .nextPoints()
      .filter(p => p.x >= 0 && p.y >= 0 && p.x <= input.maxX && p.y <= input.maxY)
      .filter(p => input.gardenPlots(p) == input.gardenPlots(currentPos))

  def sumPrice(input: Day12Input): Int =
    computeRegions(input)
      .map(r => r.area * r.perimeter)
      .sum

  def extractRegions(input: Day12Input): Seq[Set[Point2D]] = {
    val (result, _) = input.gardenPlots.keys.foldLeft((Seq.empty[Set[Point2D]], Set.empty[Point2D])) {
      case ((regionAcc, visitedAcc), currentPos) =>
        if (visitedAcc.contains(currentPos))
          (regionAcc, visitedAcc)
        else {
          val newRegion: Set[Point2D] = doExtract(input, currentPos, Set())
          (regionAcc :+ newRegion, visitedAcc ++ newRegion)
        }
    }
    result
  }

  private def doExtract(input: Day12Input, currentPos: Point2D, acc: Set[Point2D]): Set[Point2D] =
    if (acc.contains(currentPos))
      acc
    else {
      val next = nextPositions(input, currentPos)
      val result = next.foldLeft(acc) { case (newAcc, nextPos) =>
        doExtract(input, nextPos, newAcc + currentPos)
      }
      if (result.isEmpty)
        Set(currentPos)
      else
        result
    }

  def computeRegions2(input: Day12Input): Seq[Region2] = {
    val allRegions = extractRegions(input)
    allRegions.map(region => compute(input, region))
  }

  def compute(input: Day12Input, region: Set[Point2D]): Region2 = {
    val corners = countCorners(input, region, region.toList.sortBy(_.x), 0)
    Region2(input.gardenPlots(region.head), region.size, corners)
  }

  @tailrec
  def countCorners(input: Day12Input, region: Set[Point2D], remaining: List[Point2D], acc: Int): Int = remaining match {
    case Nil => acc
    case head :: xs =>
      val neighborhoods = head.nextPoints().filter(_.in(input.maxX, input.maxY)).filter(region.contains)
      if (neighborhoods.size == 4) {
        val diagonals = Seq(head.plusX(1).plusY(1), head.plusX(-1).plusY(-1), head.plusX(1).plusY(-1), head.plusX(-1).plusY(1))
        val count     = 4 - diagonals.count(region.contains)
        countCorners(input, region, xs, acc + count)
      } else if (neighborhoods.size == 3) {
        val p1 :: p2 :: p3 :: _ = neighborhoods
        val maybeDiagonal         = compute2Diagonals(head, p1, p2, p3)
        val count                 = 2 - maybeDiagonal.count(region.contains)
        countCorners(input, region, xs, acc + count)
      } else if (neighborhoods.size == 2) {
        val p1 :: p2 :: _ = neighborhoods
        if (p1.x == p2.x || p1.y == p2.y) // It is a pipe
          countCorners(input, region, xs, acc)
        else {
          val maybeDiagonal = compute1Diagonal(head, p1, p2)
          val count         = maybeDiagonal.filterNot(region.contains).map(_ => 2).getOrElse(1)
          countCorners(input, region, xs, acc + count)
        }
      } else if (neighborhoods.size == 1)
        countCorners(input, region, xs, acc + 2)
      else
        countCorners(input, region, xs, acc + 4)
  }

  private def compute1Diagonal(head: Point2D, p1: Point2D, p2: Point2D): Option[Point2D] =
    if (p1 == head.plusX(1) && p2 == head.plusY(1) || p2 == head.plusX(1) && p1 == head.plusY(1))
      Some(head.plusX(1).plusY(1))
    else if (p1 == head.plusX(-1) && p2 == head.plusY(1) || p2 == head.plusX(-1) && p1 == head.plusY(1))
      Some(head.plusX(-1).plusY(1))
    else if (p1 == head.plusX(-1) && p2 == head.plusY(-1) || p2 == head.plusX(-1) && p1 == head.plusY(-1))
      Some(head.plusX(-1).plusY(-1))
    else if (p1 == head.plusX(1) && p2 == head.plusY(-1) || p2 == head.plusX(1) && p1 == head.plusY(-1))
      Some(head.plusX(1).plusY(-1))
    else
      None

  private def compute2Diagonals(head: Point2D, p1: Point2D, p2: Point2D, p3: Point2D): Seq[Point2D] = {
    val c1 :: c2 :: c3 :: _ = List(p1, p2, p3).sortBy(p => (p.x, p.y))
    if (c1.x == head.x - 1 && c2.y == head.y - 1 && c3.x == head.x + 1)
      Seq(head.plusX(-1).plusY(-1), head.plusX(1).plusY(-1))
    else if (c1.x == head.x - 1 && c2.y == head.y - 1 && c3.y == head.y + 1)
      Seq(head.plusX(-1).plusY(-1), head.plusX(-1).plusY(1))
    else if (c1.x == head.x - 1 && c2.y == head.y + 1 && c3.x == head.x + 1)
      Seq(head.plusX(-1).plusY(1), head.plusX(1).plusY(1))
    else if (c1.y == head.y - 1 && c2.y == head.y + 1 && c3.x == head.x + 1)
      Seq(head.plusX(1).plusY(-1), head.plusX(1).plusY(1))
    else
      throw new IllegalArgumentException(s"Should not happen: head=$head p1=$p1 p2=$p2 p3=$p3")
  }

  def sumPrice2(input: Day12Input): Int =
    computeRegions2(input)
      .map(r => r.area * r.countCorners)
      .sum

}
