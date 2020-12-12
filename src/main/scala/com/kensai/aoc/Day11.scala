package com.kensai.aoc

import cats.Semigroupal
import cats.implicits._
import scala.annotation.tailrec

object Day11 {
  
  case class Pos(x: Int, y: Int) {
    def addX(amount: Int): Pos =
      copy(x = x + amount)

    def addY(amount: Int): Pos =
      copy(y = y + amount)

    def directAdjacent: List[Pos] =
      AllDirections.map(_.next(this))
  }

  case class Direction(x: Int, y: Int) {
    def next(pos: Pos): Pos =
      pos.addX(x).addY(y)

    @tailrec
    private def doNextValue(pos: Pos, board: Board): Option[Char] =
      if (isInGrid(pos, board))
        if (board.contains(pos))
          Some(board.at(pos))
        else
          doNextValue(next(pos), board)
      else
        None

    def nextValue(pos: Pos, board: Board): Option[Char] =
      doNextValue(next(pos), board)
  }

  val AllDirections: List[Direction] =
    Semigroupal.map2(List(0, 1, -1), List(0, 1, -1)){Direction}
      .filter{
        case Direction(0, 0) => false
        case _ => true
      }

  case class Board(maxX: Int, maxY: Int, grid: Map[Pos, Char]) {
    def updated(pos: Pos, value: Char): Board =
      this.copy(grid = grid.updated(pos, value))

    def at(pos: Pos): Char =
      grid(pos)

    def contains(pos: Pos): Boolean =
      grid.contains(pos)

    def countOccupied: Long =
      grid.values.count(_ == '#')
  }

  def parse(inputs: String): Board = {
    val rawRows = inputs
      .split("\n")
      .map(_.trim)
      .filterNot(_.isEmpty)
    val grid = rawRows
      .zipWithIndex
      .map(x=> x._2 -> x._1.zipWithIndex.map(y => y._2 -> y._1).filterNot(_._2 == '.').toMap)
      .flatMap{case (row, cells) => cells.map{case (col, value) => Pos(row, col) -> value}}
      .toMap

    val maxX = rawRows.length
    val maxY = rawRows.headOption.map(_.length).getOrElse(0)
    Board(maxX, maxY, grid)
  }

  def computeSwitch(pos: Pos, board: Board, expectedCount: Int, extractFun: (Pos, Board) => List[Char]): Option[(Pos, Char)] = {
    val adjacent = extractFun(pos, board)
    val count = adjacent.count(_ == '#')
    if (board.at(pos) == '#' && count >= expectedCount)
      Some (pos, 'L')
    else if (board.at(pos) == 'L' && count == 0)
      Some (pos, '#')
    else None
  }


  def doComputeNextBoard(board: Board, expectedCount: Int, extractFun: (Pos, Board) => List[Char]): Board = {
    val updates = (for {
      x <- 0 to board.maxX
      y <- 0 to board.maxY
    } yield Pos(x, y))
      .filter(board.contains)
      .flatMap(pos => computeSwitch(pos, board, expectedCount, extractFun))

    updates.foldLeft(board){case (old, ope) => old.updated(ope._1, ope._2)}
  }

  def extractAdjacent(pos: Pos, board: Board): List[Char] = {
    val positions = pos.directAdjacent
    positions.filter(isInGrid(_, board)).flatMap(board.grid.get)
  }

  def extractVisibleAdjacent(pos: Pos, board: Board): List[Char] =
    AllDirections.flatMap(d => d.nextValue(pos, board))

  def isInGrid(pos: Pos, board: Board): Boolean =
    pos.x >= 0 && pos.x <= board.maxX && pos.y >= 0 && pos.y <= board.maxY

  def countOccupiedSeats(board: Board): Long = {
    var previous = board
    var next = doComputeNextBoard(board, 4, extractAdjacent)
    while(previous != next) {
      previous = next
      next = doComputeNextBoard(next, 4, extractAdjacent)
    }

    next.countOccupied
  }

  def countOccupiedSeats2(board: Board): Long = {
    var previous = board
    var next = doComputeNextBoard(board, 5, extractVisibleAdjacent)
    while(previous != next) {
      previous = next
      next = doComputeNextBoard(next, 5, extractVisibleAdjacent)
    }

    next.countOccupied
  }
}
