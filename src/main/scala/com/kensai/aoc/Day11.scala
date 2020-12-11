package com.kensai.aoc

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

  val AllDirections = List(N, NO, NE, E, W, SW, SE, S)
  sealed trait Direction {
    def next(pos: Pos): Pos

    @tailrec
    final def nextValue(pos: Pos, board: Board): Option[Char] = {
      if (isInGrid(pos, board))
        if (board.contains(pos))
          Some(board.at(pos))
        else
        nextValue(next(pos), board)
      else
        None
    }
  }
  case object NO extends Direction {
    override def next(pos: Pos): Pos = pos.addX(-1).addY(1)
  }
  case object N extends Direction {
    override def next(pos: Pos): Pos = pos.addX(0).addY(1)
  }
  case object NE extends Direction {
    override def next(pos: Pos): Pos = pos.addX(1).addY(1)
  }
  case object E extends Direction {
    override def next(pos: Pos): Pos = pos.addX(1).addY(0)
  }
  case object W extends Direction {
    override def next(pos: Pos): Pos = pos.addX(-1).addY(0)
  }
  case object SE extends Direction {
    override def next(pos: Pos): Pos = pos.addX(1).addY(-1)
  }
  case object S extends Direction {
    override def next(pos: Pos): Pos = pos.addX(0).addY(-1)
  }
  case object SW extends Direction {
    override def next(pos: Pos): Pos = pos.addX(-1).addY(-1)
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
    AllDirections.flatMap(d => d.nextValue(d.next(pos), board))

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


//  def extractAdjacent2(x: Int, y: Int, values: List[List[String]]): List[String] = {
//    var adjacents = List.empty[String]
//    (x + 1 until values.size)
//      .map(values(_)(y))
//      .find(_ != ".")
//      .foreach(s => {
//      adjacents = s :: adjacents
//    })
//    (1 to x)
//      .map(i => x - i)
//      .filter(_ >= 0)
//      .map(values(_)(y))
//      .find(_ != ".")
//      .foreach(s => {
//      adjacents = s :: adjacents
//    })
//    (y + 1 until values(x).size)
//      .map(values(x)(_))
//      .find(_ != ".")
//      .foreach(s => {
//      adjacents = s :: adjacents
//    })
//    (1 to y)
//      .map(j => y - j)
//      .filter(_ >= 0)
//      .map(values(x)(_))
//      .find(_ != ".")
//      .foreach(s => {
//      adjacents = s :: adjacents
//    })
//    (1 until values.size)
//      .map(i => (x + i, y + i))
//      .filter{case (i, j) => i >= 0 && j >= 0}
//      .filter{case (i, j) => i < values.size && j < values(i).size}
//      .find{case (i, j) =>values(i)(j) != "."}
//      .foreach{case (i, j) => {
//        val s =  values(i)(j)
//        adjacents = s :: adjacents
//      }
//      }
//    (1 until values.size)
//      .map(i => (x - i, y - i))
//      .filter{case (i, j) => i >= 0 && j >= 0}
//      .filter{case (i, j) => i < values.size && j < values(i).size}
//      .find{case (i, j) =>values(i)(j) != "."}
//      .foreach{case (i, j) => {
//        val s =  values(i)(j)
//        adjacents = s :: adjacents
//      }
//      }
//    (1 until values.size)
//      .map(i => (x + i, y - i))
//      .filter{case (i, j) => i >= 0 && j >= 0}
//      .filter{case (i, j) => i < values.size && j < values(i).size}
//      .find{case (i, j) =>values(i)(j) != "."}
//      .foreach{case (i, j) => {
//        val s =  values(i)(j)
//        adjacents = s :: adjacents
//      }
//      }
//    (1 until values.size)
//      .map(i => (x - i, y + i))
//      .filter{case (i, j) => i >= 0 && j >= 0}
//      .filter{case (i, j) => i < values.size && j < values(i).size}
//      .find{case (i, j) =>values(i)(j) != "."}
//      .foreach{case (i, j) => {
//        val s =  values(i)(j)
//        adjacents = s :: adjacents
//      }
//      }
//    adjacents
//  }
//
//  def countOccupied(adjacent: List[String]) =
//    adjacent.count(_ == "#")
//
//
//  def doCompute(inputs: List[List[String]], expectedCount: Int)(extractFun: (Int, Int, List[List[String]]) => List[String]): List[List[String]] = {
//    var result = inputs
//    var x = 0
//    while (x < inputs.size) {
//      var y = 0
//      while (y < inputs(x).size) {
//        val cell = inputs(x)(y)
//        val adjacents = extractFun(x, y, inputs)
//        val count = countOccupied(adjacents)
//        if (cell == "L" && count == 0) {
//          val row = result(x).updated(y, "#")
//          result = result.updated(x, row)
//
//        } else if (cell == "#" && count >= expectedCount) {
//          val row = result(x).updated(y, "L")
//          result = result.updated(x, row)
//        }
//
//        y = y + 1
//      }
//      x = x + 1
//    }
//
//    result
//  }
//
//
//  def asString(values: List[List[String]]): String = {
//    (0 to values.size - 1).map(x => {
//      (0 to values(x).size - 1).map(y => {
//        values(x)(y)
//      }).mkString + "\n"
//    }).mkString
//  }
//
//  def compute1(inputs: List[List[String]]): Long = {
//    var previous = inputs
//    var next = doCompute(inputs, 4)(extractAdjacent)
//    while(previous != next) {
//      previous = next
//      next = doCompute(next, 4)(extractAdjacent)
//    }
//
//    next.flatten.count(_ == "#")
//  }
//
//  def compute2(inputs: List[List[String]]): Long = {
//    var previous = inputs
//    var next = doCompute(inputs, 5)(extractAdjacent2)
//    while(previous != next) {
//      previous = next
//      next = doCompute(next, 5)(extractAdjacent2)
//    }
//
//    next.flatten.count(_ == "#")
//  }

}
