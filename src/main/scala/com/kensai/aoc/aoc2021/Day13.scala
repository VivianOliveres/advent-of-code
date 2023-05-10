package com.kensai.aoc.aoc2021

object Day13 {

  case class PaperDot(x: Int, y: Int)

  sealed trait Fold {
    def fold(dots: Seq[PaperDot]): Seq[PaperDot]
  }
  case class XFold(x: Int) extends Fold {
    def fold(dots: Seq[PaperDot]): Seq[PaperDot] =
      dots.flatMap { d =>
        if (d.x < x) Some(d)
        else if (d.x == x) None
        else Some(PaperDot(2 * x - d.x, d.y))
      }.distinct
  }
  case class YFold(y: Int) extends Fold {
    def fold(dots: Seq[PaperDot]): Seq[PaperDot] =
      dots.flatMap { d =>
        if (d.y < y) Some(d)
        else if (d.y == y) None
        else Some(PaperDot(d.x, 2 * y - d.y))
      }.distinct
  }

  case class Inputs(dots: Seq[PaperDot], folds: Seq[Fold])

  private val dotRegex   = """(\d+),(\d+)""".r
  private val yFoldRegex = """fold along y=(\d+)""".r
  private val xFoldRegex = """fold along x=(\d+)""".r
  def parse(fileInput: String): Inputs = {
    val splitted = fileInput.split("\n\n")
    val dots = splitted.head.split("\n").collect {
      case str if str.nonEmpty => str.trim
    }.map {
      case dotRegex(x, y) => PaperDot(x.toInt, y.toInt)
      case _ =>
        throw new IllegalArgumentException(s"Invalid dot in \n${splitted.head}")
    }

    val folds = splitted(1).split("\n").collect {
      case str if str.nonEmpty => str.trim
    }.map {
      case yFoldRegex(y) => YFold(y.toInt)
      case xFoldRegex(x) => XFold(x.toInt)
      case _ =>
        throw new IllegalArgumentException(s"Invalid fold in \n${splitted(1)}")
    }
    Inputs(dots.toSeq, folds.toSeq)
  }

  /** Apply all the folds to the given dots and return the new dots.
    */
  def foldAll(inputs: Inputs): Seq[PaperDot] =
    inputs.folds.foldLeft(inputs.dots) { case (dots, f) =>
      f.fold(dots)
    }

  /** Print in the console the given dots
    */
  def printSolution(dots: Seq[PaperDot]): Unit = {
    val maxX = dots.map(_.x).max
    val maxY = dots.map(_.y).max
    for {
      y <- 0 to maxY
      x <- 0 to maxX
    } yield {
      if (dots.contains(PaperDot(x, y))) print("#")
      else print(".")
      if (x == maxX) println("")
    }
    ()
  }
}
