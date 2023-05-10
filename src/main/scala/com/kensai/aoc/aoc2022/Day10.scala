package com.kensai.aoc.aoc2022

object Day10 {

  sealed trait Instruction
  case class AddX(value: Int) extends Instruction
  case object Noop            extends Instruction

  private val addXRegex = """addx (-?\d+)""".r
  private val noopRegex = """noop""".r
  def parse(lines: Seq[String]): Seq[Instruction] =
    lines.collect {
      case str if str.nonEmpty => str.trim
    }.map {
      case noopRegex()    => Noop
      case addXRegex(str) => AddX(str.toInt)
      case str            => throw new IllegalArgumentException(s"Can not parse instruction [$str]")
    }

  def computeSignalStrength(instructions: Seq[Instruction]): Map[Int, Int] =
    instructions.foldLeft(Map(1 -> 1)) { case (acc, instr) =>
      val lastCycleNumber = acc.keySet.max
      val lastXValue      = acc(lastCycleNumber)
      instr match {
        case Noop =>
          acc + (lastCycleNumber + 1 -> lastXValue)
        case AddX(value) =>
          acc + (lastCycleNumber + 1 -> lastXValue) + (lastCycleNumber + 2 -> (value + lastXValue))
      }
    }

  def sumSignalStrength(instructions: Seq[Instruction]): Int = {
    val signalStrengths = computeSignalStrength(instructions)
    Seq(20, 60, 100, 140, 180, 220).map(i => signalStrengths(i) * i).sum
  }

  def printImage(instructions: Seq[Instruction]): String = {
    val signal = computeSignalStrength(instructions)
    val firstRow = (0 until signal.size - 1).map { case cycleNumber =>
      val x      = signal(cycleNumber + 1)
      val spritePixels = (x -1 to x + 1)
      val pixel = cycleNumber % 40
      val r =
        if (spritePixels.contains(pixel)) "#"
        else "."
      if (cycleNumber != 0 && cycleNumber % 40 == 0) "\n" + r else r
    }
    println(firstRow.mkString)
    firstRow.mkString
  }
}
