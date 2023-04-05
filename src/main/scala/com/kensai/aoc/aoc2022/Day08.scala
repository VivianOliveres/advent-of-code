package com.kensai.aoc.aoc2022

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec

object Day08 {

  type Forest = Map[Point2D, Int]
  case class InputDay8(forest: Forest, maxX: Int, maxY: Int)

  def parse(lines: Seq[String]): InputDay8 = {
    val result = for {
      (line, y) <- lines.zipWithIndex
      (cell, x) <- line.zipWithIndex
    } yield Point2D(x, y) -> cell.toString.toInt
    val forest = result.toMap
    InputDay8(forest, forest.keys.map(_.x).max, forest.keys.map(_.y).max)
  }

  def countVisibleTrees(input: InputDay8): Int = {
    val initialVisibleTress =
      (0 to input.maxX).flatMap(i => Seq(Point2D(i, 0), Point2D(i, input.maxY), Point2D(0, i), Point2D(input.maxX, i))).toSet

    val columnsByTop = (1 until input.maxX).foldLeft(initialVisibleTress) { case (acc, x) =>
      val currentMaxVisible = input.forest(Point2D(x, 0))
      val points            = doCountVisibleTreesByColumnByTop(input, x, 1, currentMaxVisible, Set())
      points ++ acc
    }
    val columnsByBottom = (1 until input.maxX).reverse.foldLeft(initialVisibleTress) { case (acc, x) =>
      val currentMaxVisible = input.forest(Point2D(x, input.maxY))
      val points            = doCountVisibleTreesByColumnByBottom(input, x, input.maxY - 1, currentMaxVisible, Set())
      points ++ acc
    }
    val rowsByLeft = (1 until input.maxY).foldLeft(initialVisibleTress) { case (acc, y) =>
      val currentMaxVisible = input.forest(Point2D(0, y))
      val points            = doCountVisibleTreesByRowByLeft(input, 1, y, currentMaxVisible, Set())
      points ++ acc
    }
    val rowsByRight = (1 until input.maxY).reverse.foldLeft(initialVisibleTress) { case (acc, y) =>
      val currentMaxVisible = input.forest(Point2D(input.maxX, y))
      val points            = doCountVisibleTreesByRowByRight(input, input.maxX - 1, y, currentMaxVisible, Set())
      points ++ acc
    }

    val result = columnsByTop ++ columnsByBottom ++ rowsByLeft ++ rowsByRight
    result.size
  }

  @tailrec
  private def doCountVisibleTreesByRowByLeft(input: InputDay8, x: Int, y: Int, maxFound: Int, acc: Set[Point2D]): Set[Point2D] = {
    val currentPoint = Point2D(x, y)
    if (x == input.maxX)
      acc
    else if (input.forest(currentPoint) > maxFound)
      doCountVisibleTreesByRowByLeft(input, x + 1, y, input.forest(currentPoint), acc + currentPoint)
    else
      doCountVisibleTreesByRowByLeft(input, x + 1, y, maxFound, acc)
  }

  @tailrec
  private def doCountVisibleTreesByRowByRight(input: InputDay8, x: Int, y: Int, maxFound: Int, acc: Set[Point2D]): Set[Point2D] = {
    val currentPoint = Point2D(x, y)
    if (x == 0)
      acc
    else if (input.forest(currentPoint) > maxFound)
      doCountVisibleTreesByRowByRight(input, x - 1, y, input.forest(currentPoint), acc + currentPoint)
    else
      doCountVisibleTreesByRowByRight(input, x - 1, y, maxFound, acc)
  }

  @tailrec
  private def doCountVisibleTreesByColumnByTop(input: InputDay8, x: Int, y: Int, maxFound: Int, acc: Set[Point2D]): Set[Point2D] = {
    val currentPoint = Point2D(x, y)
    if (y == input.maxX)
      acc
    else if (input.forest(currentPoint) > maxFound)
      doCountVisibleTreesByColumnByTop(input, x, y + 1, input.forest(currentPoint), acc + currentPoint)
    else
      doCountVisibleTreesByColumnByTop(input, x, y + 1, maxFound, acc)
  }

  @tailrec
  private def doCountVisibleTreesByColumnByBottom(input: InputDay8, x: Int, y: Int, maxFound: Int, acc: Set[Point2D]): Set[Point2D] = {
    val currentPoint = Point2D(x, y)
    if (y == 0)
      acc
    else if (input.forest(currentPoint) > maxFound)
      doCountVisibleTreesByColumnByBottom(input, x, y - 1, input.forest(currentPoint), acc + currentPoint)
    else
      doCountVisibleTreesByColumnByBottom(input, x, y - 1, maxFound, acc)
  }

  def highestTreeScenicScore(input: InputDay8): Int =
    input.forest.keys.map(tree => (tree, treeScenicScore(input, tree))).maxBy(_._2)._2

  @tailrec
  private def doComputeLeftVisibleTrees(input: InputDay8, treeValue: Int, currentPoint: Point2D, acc: Set[Point2D]): Set[Point2D] =
    if (currentPoint.x < 0)
      acc
    else if (input.forest(currentPoint) >= treeValue)
      acc + currentPoint
    else if (input.forest(currentPoint) < treeValue)
      doComputeLeftVisibleTrees(input, treeValue, currentPoint.minusX(1), acc + currentPoint)
    else
      acc

  @tailrec
  private def doComputeRightVisibleTrees(input: InputDay8, treeValue: Int, currentPoint: Point2D, acc: Set[Point2D]): Set[Point2D] =
    if (currentPoint.x > input.maxX)
      acc
    else if (input.forest(currentPoint) >= treeValue)
      acc + currentPoint
    else if (input.forest(currentPoint) < treeValue)
      doComputeRightVisibleTrees(input, treeValue, currentPoint.plusX(1), acc + currentPoint)
    else
      acc

  @tailrec
  private def doComputeTopVisibleTrees(input: InputDay8, treeValue: Int, currentPoint: Point2D, acc: Set[Point2D]): Set[Point2D] =
    if (currentPoint.y < 0)
      acc
    else if (input.forest(currentPoint) >= treeValue)
      acc + currentPoint
    else if (input.forest(currentPoint) < treeValue)
      doComputeTopVisibleTrees(input, treeValue, currentPoint.minusY(1), acc + currentPoint)
    else
      acc

  @tailrec
  private def doComputeBottomVisibleTrees(input: InputDay8, treeValue: Int, currentPoint: Point2D, acc: Set[Point2D]): Set[Point2D] =
    if (currentPoint.y > input.maxY)
      acc
    else if (input.forest(currentPoint) >= treeValue)
      acc + currentPoint
    else if (input.forest(currentPoint) < treeValue)
      doComputeBottomVisibleTrees(input, treeValue, currentPoint.plusY(1), acc + currentPoint)
    else
      acc

  def treeScenicScore(input: InputDay8, tree: Point2D): Int = {
    val treeValue          = input.forest(tree)
    val leftVisibleTrees   = doComputeLeftVisibleTrees(input, treeValue, tree.minusX(1), Set())
    val rightVisibleTrees  = doComputeRightVisibleTrees(input, treeValue, tree.plusX(1), Set())
    val topVisibleTrees    = doComputeTopVisibleTrees(input, treeValue, tree.minusY(1), Set())
    val bottomVisibleTrees = doComputeBottomVisibleTrees(input, treeValue, tree.plusY(1), Set())
    leftVisibleTrees.size * rightVisibleTrees.size * topVisibleTrees.size * bottomVisibleTrees.size
  }

}
