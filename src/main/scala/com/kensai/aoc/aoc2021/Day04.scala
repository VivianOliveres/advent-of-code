package com.kensai.aoc.aoc2021

import scala.annotation.tailrec

object Day04 {

  case class BingoGame(drawNumbers: Array[Int], boards: Array[BingoBoard])
  case class BingoBoard(cells: Array[Array[BingoCell]])
  case class BingoCell(value: Int, isMarked: Boolean = false)

  /** Parse file into a BingoGame.
    */
  def parse(input: String): BingoGame = {
    val split       = input.split("\n\n")
    val drawNumbers = split.head.split(",").map(_.toInt)

    val boards = split.tail.filterNot(_.isEmpty).map { boardString =>
      val board = boardString.split("\n").filterNot(_.isEmpty).map { line =>
        line.split(" ").filterNot(_.isEmpty).map(_.toInt).map(BingoCell(_))
      }
      BingoBoard(board)
    }

    BingoGame(drawNumbers, boards)
  }

  /** Parse the file into a BingoGame, then play until one board win and return the score of this winning board.
    */
  def computeBestBoardScore(input: String): Long = {
    val game = parse(input)
    playGame(game)
  }

  @tailrec
  private def playGame(game: BingoGame): Long = {
    val drawnNumber                                 = game.drawNumbers.head
    val updatedBoards: Array[(Boolean, BingoBoard)] = game.boards.map(play(_, drawnNumber))
    val maybeWinningSum                             = updatedBoards.find(_._1)
    if (maybeWinningSum.isDefined) {
      computeSumUnmarkedCell(maybeWinningSum.get._2) * drawnNumber.toLong
    } else {
      playGame(game.copy(game.drawNumbers.tail, updatedBoards.map(_._2)))
    }
  }

  /** Play the dransNumber on this board.
    * @return
    *   an updated board (ie with cells marked) and a boolean indicating if this board has win the game.
    */
  private def play(board: BingoBoard, drawnNumber: Int): (Boolean, BingoBoard) = {
    val updatedBoard = board.cells.map { line =>
      line.map(cell => if (cell.value == drawnNumber) cell.copy(isMarked = true) else cell)
    }
    val isWinning = hasWinningLine(updatedBoard) || hasWinningColumn(updatedBoard)
    (isWinning, BingoBoard(updatedBoard))
  }

  private def hasWinningLine(cells: Array[Array[BingoCell]]): Boolean =
    cells.exists(_.forall(_.isMarked))

  private def hasWinningColumn(cells: Array[Array[BingoCell]]): Boolean =
    cells.head.indices.exists(column => cells.forall(line => line(column).isMarked))

  private def computeSumUnmarkedCell(board: BingoBoard): Int =
    board.cells.flatten.filterNot(_.isMarked).map(_.value).sum

  /** Parse the file into a BingoGame, then play until ALL boards win and return the score of the last winning board.
    */
  def computeWorstBoardScore(input: String): Long = {
    val game = parse(input)
    playTillEndGame(game)
  }

  @tailrec
  private def playTillEndGame(game: BingoGame): Long = {
    val drawnNumber                                 = game.drawNumbers.head
    val updatedBoards: Array[(Boolean, BingoBoard)] = game.boards.map(play(_, drawnNumber))
    val maybeWinningSum                             = updatedBoards.find(_._1)
    if (maybeWinningSum.isDefined && updatedBoards.length == 1) {
      computeSumUnmarkedCell(maybeWinningSum.get._2) * drawnNumber.toLong
    } else {
      playTillEndGame(game.copy(game.drawNumbers.tail, updatedBoards.filterNot(_._1).map(_._2)))
    }
  }

}
