package com.kensai.aoc.aoc2021

import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Pairs
import enumeratum._

import scala.annotation.tailrec
import scala.collection.mutable

/** Dijkstra's algo
  */
object Day23 {

  sealed abstract class Amphipod(override val entryName: String, val energy: Int, val roomIndex: Int) extends EnumEntry {
    def name: String =
      toString.head.toString
  }
  object Amphipod extends Enum[Amphipod] {
    val values = findValues

    case object Amber  extends Amphipod("A", energy = 1, roomIndex = 2)
    case object Bronze extends Amphipod(entryName = "B", energy = 10, roomIndex = 4)
    case object Copper extends Amphipod("C", energy = 100, roomIndex = 6)
    case object Desert extends Amphipod("D", energy = 1000, roomIndex = 8)

  }

  type Positions = Map[Point2D, Amphipod]
  case class Board(positions: Positions, totalEnergySpent: Int, roomSize: Int, possiblePoints: Seq[Point2D]) {
    def isEmpty(position: Point2D): Boolean =
      !positions.contains(position)
  }

  def parsePart1(rows: Seq[String]): Board = {
    val parseRow: String => Seq[Amphipod] = row =>
      row.filterNot(_ == '#').filterNot(_ == ' ').split("").toSeq.map(c => Amphipod.withName(c))

    val positions = for {
      y             <- 2 to 3
      (amphipod, x) <- parseRow(rows(y)).zipWithIndex
    } yield (Point2D(2 + x * 2, y - 1), amphipod)

    val points = Seq(
      Point2D(0, 0),
      Point2D(1, 0),
      Point2D(3, 0),
      Point2D(5, 0),
      Point2D(7, 0),
      Point2D(9, 0),
      Point2D(10, 0),
      Point2D(2, 1),
      Point2D(4, 1),
      Point2D(6, 1),
      Point2D(8, 1),
      Point2D(2, 2),
      Point2D(4, 2),
      Point2D(6, 2),
      Point2D(8, 2)
    )
    Board(positions.toMap, 0, 2, points)
  }

  def parsePart2(rows: Seq[String]): Board = {
    val board = parsePart1(rows)
    val moved = board.positions.map {
      case (Point2D(x, 2), amphipod) => (Point2D(x, 4), amphipod)
      case (p, amphipod)             => (p, amphipod)
    }
    // #D#C#B#A#
    // #D#B#A#C#
    import com.kensai.aoc.aoc2021.Day23.Amphipod._
    val newPositions = moved ++ Map(
      Point2D(2, 2) -> Desert,
      Point2D(2, 3) -> Desert,
      Point2D(4, 2) -> Copper,
      Point2D(4, 3) -> Bronze,
      Point2D(6, 2) -> Bronze,
      Point2D(6, 3) -> Amber,
      Point2D(8, 2) -> Amber,
      Point2D(8, 3) -> Copper
    )
    val points = Seq(
      Point2D(0, 0),
      Point2D(1, 0),
      Point2D(3, 0),
      Point2D(5, 0),
      Point2D(7, 0),
      Point2D(9, 0),
      Point2D(10, 0),
      Point2D(2, 1),
      Point2D(4, 1),
      Point2D(6, 1),
      Point2D(8, 1),
      Point2D(2, 2),
      Point2D(4, 2),
      Point2D(6, 2),
      Point2D(8, 2),
      Point2D(2, 3),
      Point2D(4, 3),
      Point2D(6, 3),
      Point2D(8, 3),
      Point2D(2, 4),
      Point2D(4, 4),
      Point2D(6, 4),
      Point2D(8, 4)
    )
    Board(newPositions, 0, 4, points)
  }

  def computeBestSolution(board: Board): Int = {
    // Cache for the path between two points to avoid to recompute them (and generate lots of temporary objects)
    val paths            = computePaths(board.possiblePoints)
    val bestPositions    = mutable.Map(board.positions -> 0)
    val nextPositions    = mutable.PriorityQueue(board)(Ordering.by(b => -b.totalEnergySpent))
    val totalEnergySpent = doComputeBestSolution(paths, bestPositions, nextPositions)
    totalEnergySpent
  }

  @tailrec
  private def doComputeBestSolution(
      paths: Map[(Point2D, Point2D), Seq[Point2D]],
      bestPositions: mutable.Map[Positions, Int],
      nextPositions: mutable.PriorityQueue[Board]
    ): Int = {
    val currentBoard = nextPositions.dequeue()
    if (isFinal(currentBoard))
      currentBoard.totalEnergySpent
    else if (bestPositions(currentBoard.positions) < currentBoard.totalEnergySpent)
      doComputeBestSolution(paths, bestPositions, nextPositions)
    else {
      val newBoards = computeNextBoards(currentBoard, paths)
      newBoards
        .filter(b => !bestPositions.contains(b.positions) || bestPositions(b.positions) > b.totalEnergySpent)
        .foreach { b =>
          bestPositions.put(b.positions, b.totalEnergySpent)
          nextPositions.enqueue(b)
          ()
        }
      doComputeBestSolution(paths, bestPositions, nextPositions)
    }
  }

  private def isFinal(board: Board): Boolean =
    board.positions.forall { case (point, amphipod) => point.x == amphipod.roomIndex }

  private def computeNextBoards(board: Board, paths: Map[(Point2D, Point2D), Seq[Point2D]]): Iterable[Board] = {
    val results = for {
      (currentPosition, amphipod) <- board.positions
      nextPosition                <- nextPossiblePositions(board, currentPosition, amphipod)
      if board.isEmpty(nextPosition)
      path = paths((currentPosition, nextPosition))
      if path.forall(board.isEmpty) // Path is free
      energyToSpend = path.size * amphipod.energy
    } yield {
      val newPositions = board.positions - currentPosition + (nextPosition -> amphipod)
      val newEnergy    = board.totalEnergySpent + energyToSpend
      Board(newPositions, newEnergy, board.roomSize, board.possiblePoints)
    }
    results
  }

  private val hallPositions = (0 to 10)
    .filterNot(x => x == 2 || x == 4 || x == 6 || x == 8) // Entry of the room
    .map(x => Point2D(x, 0))

  private def roomPositions(roomIndex: Int, roomSize: Int): Seq[Point2D] =
    (1 to roomSize).map(Point2D(roomIndex, _))

  private def nextPossiblePositions(board: Board, currentPosition: Point2D, amphipod: Amphipod): Seq[Point2D] = {
    val result = if (currentPosition.x == amphipod.roomIndex) {
      val allOks = (currentPosition.y + 1 to board.roomSize)
        .map(Point2D(currentPosition.x, _))
        .forall(p => !board.isEmpty(p) && board.positions(p) == amphipod)
      if (allOks) // At a good position => do not move
        Seq()
      else // There is a bad amphipod higher in the room
        hallPositions
    } else if (currentPosition.y == 0) { // from hall to room
      val rooms = roomPositions(amphipod.roomIndex, board.roomSize)
      if (rooms.exists(p => !board.isEmpty(p) && board.positions(p) != amphipod)) // not empty and invalid amphipod
        Seq()
      else { // empty or good amphipod
        rooms
      }
    } else // From invalid room to hall
      hallPositions

    result
  }

  private def computePaths(points: Seq[Point2D]): Map[(Point2D, Point2D), Seq[Point2D]] =
    Pairs.generateAllPairs(points).map { case (from, to) => (from, to) -> getPath(from, to) }.toMap

  private def getPath(from: Point2D, to: Point2D): Seq[Point2D] = {
    val hall =
      if (from.x < to.x)
        (from.x + 1 to to.x).map(x => Point2D(x, 0))
      else
        (to.x until from.x).map(x => Point2D(x, 0))

    val rooms =
      if (from.y == 0)
        (1 to to.y).map(y => Point2D(to.x, y))
      else
        (0 until from.y).map(y => Point2D(from.x, y))

    (rooms ++ hall).distinct
  }

  def print(board: Board): Unit = {
    val hallStr = toStringHall(board)
    println(s"Energy Spent [${board.totalEnergySpent}]")
    println(hallStr)
    (1 to board.roomSize).foreach(i => println(toStringRoom(board, i)))
  }

  private def toStringHall(board: Board): String =
    (0 to 10)
      .map(x => board.positions.get(Point2D(x, 0)))
      .map(maybeAmphipod => maybeAmphipod.map(_.name).getOrElse("."))
      .mkString("")

  private def toStringRoom(board: Board, roomIndex: Int): String =
    (0 to 10)
      .map {
        case 0 | 10            => " "
        case 1 | 3 | 5 | 7 | 9 => "|"
        case x                 => board.positions.get(Point2D(x, roomIndex)).map(_.name).getOrElse(".")
      }
      .mkString("")

  def print(before: Board, after: Board): Unit = {
    val hallStr = toStringHall(before) + " | " + toStringHall(after)
    println(hallStr)
    (1 to before.roomSize).foreach { i =>
      val line = toStringRoom(before, i) + " | " + toStringRoom(after, i)
      println(line)
    }
  }

}
