package com.kensai.aoc.aoc2022

import com.kensai.aoc.lib.Geo.Point2D

object Day05 {

  case class MoveInstruction(count: Int, from: Int, to: Int)

  case class InputDay5(stacks: Map[Point2D, Char], instructions: Seq[MoveInstruction])

  private val instructionRegex = """move (\d+) from (\d+) to (\d+)""".r
  def parse(input: String): InputDay5 = {
    val (first :: second :: _) = input.split("\n\n").toList

    val diagram = first.split("\n").toSeq.reverse
    val stacks = diagram.tail.zipWithIndex
      .map { case (row, y) =>
        val toto = for {
          xCrane <- 0 to row.length
          xRow = xCrane * 4 + 1 if row.length > xRow
          cell = row.charAt(xRow) if cell != ' '
        } yield Point2D(xCrane + 1, y + 1) -> cell
        toto.toMap
      }
      .foldLeft(Map.empty[Point2D, Char])(_ ++ _)

    val instructions = second.split("\n").toSeq.filterNot(_.isEmpty).map {
      case instructionRegex(countStr, fromStr, toStr) => MoveInstruction(countStr.toInt, fromStr.toInt, toStr.toInt)
      case row                                        => throw new IllegalArgumentException(s"Invalid row [$row]")
    }

    InputDay5(stacks, instructions)
  }

  def executePart1(input: InputDay5): InputDay5 =
    executePart(input, true)
  def executePart2(input: InputDay5): InputDay5 =
    executePart(input, false)

  def executePart(input: InputDay5, isPart1: Boolean): InputDay5 = {
    val instr    = input.instructions.head
    val maxYFrom = input.stacks.keys.filter(_.x == instr.from).map(_.y).maxOption.getOrElse(0)
    val maxYTo   = input.stacks.keys.filter(_.x == instr.to).map(_.y).maxOption.getOrElse(0)
    val toAddAndRemove: Seq[((Point2D, Char), Point2D)] = for {
      i <- 1 to instr.count
      from = Point2D(instr.from, maxYFrom + 1 - i)
      toY  = if (isPart1) maxYTo + i else maxYTo + instr.count + 1 - i
      to   = Point2D(instr.to, toY)
    } yield (to -> input.stacks(from), from)
    val newStack = toAddAndRemove.foldLeft(input.stacks) { case (acc, (toAdd, toRemove)) =>
      acc - toRemove + toAdd
    }

    InputDay5(newStack, input.instructions.tail)
  }

  def executeAll(input: InputDay5, isPart1: Boolean): String = {
    val finalState = input.instructions.foldLeft(input) { case (acc, _) =>
      executePart(acc, isPart1)
    }
    val higherCranesByX = finalState.stacks.toSeq
      .groupBy(_._1.x)
      .map { case (x, values) =>
        (x, values.maxBy(_._1.y)._2)
      }
    (1 to higherCranesByX.size)
      .map(x => higherCranesByX(x))
      .mkString
  }

}
