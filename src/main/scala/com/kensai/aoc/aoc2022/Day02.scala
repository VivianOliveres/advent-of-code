package com.kensai.aoc.aoc2022

object Day02 {

  sealed trait Choosed {
    val scoreValue: Int
    val wins: Choosed
    val loses: Choosed
  }

  case object Rock extends Choosed {
    val scoreValue: Int = 1
    val wins: Choosed   = Scissors
    val loses: Choosed  = Paper
  }
  case object Paper extends Choosed {
    val scoreValue: Int = 2
    val wins: Choosed   = Rock
    val loses: Choosed  = Scissors
  }
  case object Scissors extends Choosed {
    val scoreValue: Int = 3
    val wins: Choosed   = Paper
    val loses: Choosed  = Rock
  }

  case class Round(opponent: Choosed, player: Choosed) {

    val score: Int = {
      val matchScore = if (player == opponent) 3 else if (player.wins == opponent) 6 else 0
      matchScore + player.scoreValue
    }
  }

  private val lineRegex = "(\\w) (\\w)".r
  def parse1(lines: Seq[String]): Seq[Round] =
    lines
      .collect {
        case str if str.nonEmpty => str.trim
      }
      .map {
        case lineRegex(opponentStr, playerStr) => Round(fromOpponent(opponentStr), fromPlayer(playerStr))
        case str                               => throw new IllegalArgumentException(s"Invalid line [$str]")
      }

  def parse2(lines: Seq[String]): Seq[Round] =
    lines
      .collect {
        case str if str.nonEmpty => str.trim
      }
      .map {
        case lineRegex(opponentStr, playerStr) =>
          val opponent = fromOpponent(opponentStr)
          Round(opponent, fromGame(opponent, playerStr))
        case str => throw new IllegalArgumentException(s"Invalid line [$str]")
      }

  private def fromOpponent(str: String): Choosed = str match {
    case "A" => Rock
    case "B" => Paper
    case "C" => Scissors
    case _   => throw new IllegalArgumentException(s"Invalid fromOpponent input [$str]")
  }
  private def fromPlayer(str: String): Choosed = str match {
    case "X" => Rock
    case "Y" => Paper
    case "Z" => Scissors
    case _   => throw new IllegalArgumentException(s"Invalid fromPlayer input [$str]")
  }

  private def fromGame(opponent: Choosed, str: String): Choosed = str match {
    case "X" => opponent.wins
    case "Y" => opponent
    case "Z" => opponent.loses
    case _   => throw new IllegalArgumentException(s"Invalid fromGame input [$str]")
  }

  def computeScore(rounds: Seq[Round]): Int =
    rounds.map(_.score).sum

}
