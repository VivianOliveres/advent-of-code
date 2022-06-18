package com.kensai.aoc.aoc2020

object Day17 {

  case class Coord3(x: Int, y: Int, z: Int) {
    def toCoord4(w: Int = 0): Coord4 = Coord4(x, y, z, w)
  }
  case class Coord4(x: Int, y: Int, z: Int, w: Int)

  def parse3(inputs: String): Set[Coord3] =
    inputs
      .split("\n")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .zipWithIndex
      .flatMap {
        case (row, y) => {
          row
            .chars()
            .toArray
            .toList
            .zipWithIndex
            .filter(_._1 == '#')
            .map { case (_, x) => Coord3(x, y, 0) }
        }
      }
      .toSet

  def parse4(inputs: String): Set[Coord4] =
    parse3(inputs).map(_.toCoord4())

  def computeNeighbors3(coord: Coord3): Seq[Coord3] =
    for {
      x <- coord.x - 1 to coord.x + 1
      y <- coord.y - 1 to coord.y + 1
      z <- coord.z - 1 to coord.z + 1
      if !(x == coord.x && y == coord.y && z == coord.z)
    } yield Coord3(x, y, z)

  def computeNeighbors4(coord: Coord4): Seq[Coord4] =
    for {
      x <- coord.x - 1 to coord.x + 1
      y <- coord.y - 1 to coord.y + 1
      z <- coord.z - 1 to coord.z + 1
      w <- coord.w - 1 to coord.w + 1
      if !(x == coord.x && y == coord.y && z == coord.z && w == coord.w)
    } yield Coord4(x, y, z, w)

  def isCubeActivated[T](
      coord: T,
      activeCoord: Set[T],
      neighbors: Int
  ): Boolean =
    if (activeCoord.contains(coord))
      neighbors == 2 || neighbors == 3
    else
      neighbors == 3

  def doComputeNextState[T](
      activeCoord: Set[T],
      neighborsGenerator: T => Seq[T]
  ): Set[T] =
    activeCoord.toList
      .flatMap(neighborsGenerator)
      .groupBy(x => x)
      .map { case (coord, all) => (coord, all.size) }
      .filter { case (coord, size) =>
        isCubeActivated(coord, activeCoord, size)
      }
      .keySet

  def activeCubeAfter3(input: String, numCycles: Int): Long =
    activeCubeAfter(computeNeighbors3)(numCycles, parse3(input))

  def activeCubeAfter4(input: String, numCycles: Int): Long =
    activeCubeAfter(computeNeighbors4)(numCycles, parse4(input))

  def activeCubeAfter[T](
      neighborsGenerator: T => Seq[T]
  )(numCycles: Int, cubes: Set[T]): Long =
    (0 until numCycles)
      .foldLeft(cubes) { case (acc, _) =>
        doComputeNextState(acc, neighborsGenerator)
      }
      .size
      .toLong
}
