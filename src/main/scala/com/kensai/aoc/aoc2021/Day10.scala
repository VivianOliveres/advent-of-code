package com.kensai.aoc.aoc2021

object Day10 {

  case class Chunk(
      open: Char,
      close: Char,
      corruptedScore: Int,
      incompleteScore: Int)
  val allChunks: Seq[Chunk] = Seq(
    Chunk('(', ')', 3, 1),
    Chunk('{', '}', 1197, 3),
    Chunk('[', ']', 57, 2),
    Chunk('<', '>', 25137, 4)
  )

  private def parseLines(lines: Seq[String]): Seq[Seq[Char]] =
    lines.collect {
      case str if str.nonEmpty => str.trim
    }.map(_.toCharArray.toSeq)

  def computeCorruptedSyntaxScore(lines: Seq[String]): Int = {
    val inputs = parseLines(lines)
    inputs.map(line => doComputeCorruptedSyntaxScore(line, Seq())).sum
  }

  private def doComputeCorruptedSyntaxScore(
      line: Seq[Char],
      openChars: Seq[Char]
    ): Int = line match {
    case Nil => 0
    case head +: tail =>
      val chunk = allChunks.find(c => c.open == head || c.close == head).get
      if (chunk.open == head)
        doComputeCorruptedSyntaxScore(tail, head +: openChars)
      else {
        if (openChars.head == chunk.open) {
          doComputeCorruptedSyntaxScore(tail, openChars.tail)
        } else {
          chunk.corruptedScore
        }
      }
    case _ =>
      ???
  }

  def doComputeIncompleteSyntaxScore(
      line: Seq[Char],
      openChars: Seq[Char]
    ): Seq[Int] = line match {
    case Nil =>
      openChars
        .flatMap(c => allChunks.find(_.open == c))
        .map(chunk => chunk.incompleteScore)
    case head +: tail =>
      val chunk = allChunks.find(c => c.open == head || c.close == head).get
      if (chunk.open == head)
        doComputeIncompleteSyntaxScore(tail, head +: openChars)
      else {
        if (openChars.head == chunk.open) {
          doComputeIncompleteSyntaxScore(tail, openChars.tail)
        } else {
          Seq() // Corrupted line
        }
      }
    case _ =>
      // Should not happen
      ???
  }

  def computeIncompleteSyntaxScore(lines: Seq[String]): Seq[Long] = {
    val inputs = parseLines(lines)
    val incompleteLines = inputs
      .map(line => doComputeIncompleteSyntaxScore(line, Seq()))
      .filterNot(_.isEmpty)
    incompleteLines.map(incompleteLine => incompleteLine.foldLeft(0L) { case (acc, right) => 5L * acc + right })
  }

  def computeMiddleIncompleteSyntaxScore(lines: Seq[String]): Long = {
    val incompleteScores = computeIncompleteSyntaxScore(lines)
    incompleteScores.sorted.apply(incompleteScores.size / 2)
  }

}
