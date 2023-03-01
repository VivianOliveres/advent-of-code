package com.kensai.aoc.aoc2020

import scala.annotation.tailrec

object Day08 {

  sealed abstract class Instruction(val index: Int, val value: Int)
  case class Jump(override val index: Int, override val value: Int) extends Instruction(index, value)
  case class Acc(override val index: Int, override val value: Int) extends Instruction(index, value)
  case class Nop(override val index: Int, override val value: Int) extends Instruction(index, value)

  private val rowRegex = """([a-z]+) ([+-]\d+)""".r

  def parseRows(input: List[String]): List[Instruction] =
    input
      .map(_.trim)
      .filterNot(_.isEmpty)
      .zipWithIndex
      .map(t => parseRow(t._1, t._2))

  def parseRow(input: String, index: Int): Instruction =
    input match {
      case rowRegex(str, value) =>
        str match {
          case "acc" => Acc(index, value.toInt)
          case "nop" => Nop(index, value.toInt)
          case "jmp" => Jump(index, value.toInt)
          case _     => throw new RuntimeException(s"Unknown action: [$input]")
        }
      case _ => throw new IllegalArgumentException(s"Invalid row [$input]")
    }

  def accumulatorBeforeLoop(inputs: List[String]): Long =
    accumulatorBeforeLoop(0, parseRows(inputs), 0L, Set())

  @tailrec
  private def accumulatorBeforeLoop(
      index: Int,
      parsed: List[Instruction],
      acc: Long,
      indexesVisited: Set[Int]
  ): Long = {
    val current = parsed(index)
    if (indexesVisited.contains(current.index))
      acc // Loop reached: return solution
    else {
      val updatedIndexesVisited = indexesVisited + current.index
      current match { // Next step
        case Nop(_, _) =>
          accumulatorBeforeLoop(index + 1, parsed, acc, updatedIndexesVisited)
        case Acc(_, value) =>
          accumulatorBeforeLoop(
            index + 1,
            parsed,
            acc + value,
            updatedIndexesVisited
          )
        case Jump(_, value) =>
          accumulatorBeforeLoop(
            index + value,
            parsed,
            acc,
            updatedIndexesVisited
          )
      }
    }
  }

  def accumulatorAfterFixingInputs(inputs: List[String]): Long = {
    val parsed = parseRows(inputs)
    inputs.indices
      .map(index =>
        parsed.updated(index, switchValues(parsed(index)))
      ) // Generate data set
      .map(s =>
        accumulatorAfterFixingInputs(0, s, 0L, Set())
      ) // compute solution
      .filterNot(_ < 0) // Filter invalid
      .head
  }

  private def switchValues(instruction: Instruction): Instruction =
    instruction match {
      case Jump(index, value) => Nop(index, value)
      case Nop(index, value)  => Jump(index, value)
      case _                  => instruction
    }

  @tailrec
  private def accumulatorAfterFixingInputs(
      currentIndex: Int,
      parsed: List[Instruction],
      acc: Long,
      indexVisited: Set[Int]
  ): Long =
    if (currentIndex == parsed.size)
      acc // Solution
    else if (currentIndex < 0 || currentIndex > parsed.size)
      -1 // Invalid
    else {
      val current = parsed(currentIndex)
      if (indexVisited.contains(current.index))
        -1 // already visited
      else {
        val updatedIndexVisited = indexVisited + current.index
        current match { // Next step
          case Jump(index, value) =>
            accumulatorAfterFixingInputs(
              index + value,
              parsed,
              acc,
              updatedIndexVisited
            )
          case Nop(index, _) =>
            accumulatorAfterFixingInputs(
              index + 1,
              parsed,
              acc,
              updatedIndexVisited
            )
          case Acc(index, value) =>
            accumulatorAfterFixingInputs(
              index + 1,
              parsed,
              acc + value,
              updatedIndexVisited
            )
        }
      }
    }

}
