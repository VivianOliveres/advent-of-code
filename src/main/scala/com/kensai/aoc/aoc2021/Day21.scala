package com.kensai.aoc.aoc2021

import scala.annotation.tailrec

object Day21 {

  case class Player(position: Int, score: Int = 0)

  case class Day21Input(rollCount: Int, player1: Player, player2: Player) {
    def hasWinner(winningScore: Int): Boolean =
      player2.score >= winningScore || player1.score >= winningScore

    def loser(winningScore: Int): Player =
      if(player1.score >= winningScore) player2 else player1
  }

  def parse(rows: Seq[String]): Day21Input = {
    val player1Position :: player2Position :: _ = rows.map(_.split(" ")).map(split => split(split.length - 1).toInt).take(2)
    Day21Input(0, Player(player1Position), Player(player2Position))
  }

  def firstPlayerToPlay(rollCount: Int): Boolean =
    rollCount % 6 == 0

  @tailrec
  def movePlayer(currentPosition: Int, steps: Int): Int =
    if (steps < 11)
      (currentPosition + steps) % 10 match {
        case 0 => 10
        case i => i
      }
    else
      movePlayer(currentPosition, steps - 10)

  def move(input: Day21Input, dicedValuesToAdd: Int): Day21Input = {
    val isFirstPlayerToPlay = firstPlayerToPlay(input.rollCount)
    val player = if (isFirstPlayerToPlay) input.player1 else input.player2
    val newPosition = movePlayer(player.position, dicedValuesToAdd)
    val newScore = player.score + newPosition
    if (isFirstPlayerToPlay)
      input.copy(rollCount = input.rollCount + 3, player1 = Player(newPosition, newScore))
    else
      input.copy(rollCount = input.rollCount + 3, player2 = Player(newPosition, newScore))
  }

  def roll(rollCount: Int, diceSides: Int): Int =
    rollCount % diceSides match {
      case 0 => diceSides
      case i => i
    }

  def rollAndMove(input: Day21Input, diceSides: Int): Day21Input = {
    val dicedValuesToAdd = (input.rollCount + 1 to input.rollCount + 3).map(roll(_, diceSides)).sum
    move(input, dicedValuesToAdd)
  }

  def findSolution(input: Day21Input, winningScore: Int, diceSides: Int): Long = {
    val result = doFindSolution(input, winningScore, diceSides)
    result.loser(winningScore).score.toLong * result.rollCount.toLong
  }

  @tailrec
  private def doFindSolution(input: Day21Input, winningScore: Int, diceSides: Int): Day21Input =
    if (input.hasWinner(winningScore))
      input
    else
      doFindSolution(rollAndMove(input, diceSides), winningScore, diceSides)

  case class GameWinCounter(player1Count: Long, player2Count: Long) {
    def incPlayer1(value: Long): GameWinCounter =
      GameWinCounter(player1Count + value , player2Count)
    def incPlayer2(value: Long): GameWinCounter =
      GameWinCounter(player1Count, player2Count + value)
    def max: Long = math.max(player1Count, player2Count)
  }

  private val diracRollCounts =
    (for {
      r1 <- 1 to 3
      r2 <- 1 to 3
      r3 <- 1 to 3
    } yield r1 + r2 + r3)
      .groupBy(identity)
      .map(entry => (entry._1, entry._2.size.toLong))

  def findQuantumSolution(input: Day21Input, winningScore: Int): Long = {
    val (counter, _) = doFindQuantumSolution(winningScore, GameWinCounter(0, 0), Map(input -> 1L))
    counter.max
  }

  @tailrec
  private def doFindQuantumSolution(winningScore: Int, counter: GameWinCounter, acc: Map[Day21Input, Long]): (GameWinCounter, Map[Day21Input, Long]) = {
    if (acc.isEmpty)
      (counter, acc)
    else {
      val ((current, count), next) = (acc.head, acc.tail)
      if (current.player2.score >= winningScore || current.player1.score >= winningScore) {
        val newCounter = if (firstPlayerToPlay(current.rollCount)) counter.incPlayer1(count) else counter.incPlayer2(count)
        doFindQuantumSolution(winningScore, newCounter, next)
      } else {
        val toUpdate = diracRollCounts
          .map(value => move(current, value._1) -> value._2)
          .map{ case (i, countLocal) =>
            (i, count * countLocal + next.getOrElse(i, 0L))
          }
        doFindQuantumSolution(winningScore, counter, next ++ toUpdate)
      }
    }

  }
}
