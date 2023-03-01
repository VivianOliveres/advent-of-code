package com.kensai.aoc.aoc2021

object Day02 {

  trait SubmarineCmd
  case class ForwardCmd(value: Int) extends SubmarineCmd
  case class DownCmd(value: Int)    extends SubmarineCmd
  case class UpCmd(value: Int)      extends SubmarineCmd

  case class SubmarinePos(horizontal: Int, depth: Int, aim: Int = 0) {
    def execute(cmd: SubmarineCmd): SubmarinePos = cmd match {
      case ForwardCmd(value) => SubmarinePos(horizontal + value, depth)
      case DownCmd(value)    => SubmarinePos(horizontal, depth + value)
      case UpCmd(value)      => SubmarinePos(horizontal, depth - value)
      case _                 => this
    }

    def execute2(cmd: SubmarineCmd): SubmarinePos = cmd match {
      case ForwardCmd(value) => SubmarinePos(horizontal + value, depth + aim * value, aim)
      case DownCmd(value)    => SubmarinePos(horizontal, depth, aim + value)
      case UpCmd(value)      => SubmarinePos(horizontal, depth, aim - value)
      case _                 => this
    }
  }

  /** Compute the position of the submarine after applying series of commands.
    */
  def computePosition(inputs: Seq[String]): Int = {
    val commands = inputs.flatMap(parseCommand)
    val pos      = doComputePosition(commands)
    pos.depth * pos.horizontal
  }

  private def doComputePosition(commands: Seq[SubmarineCmd]): SubmarinePos =
    commands.foldLeft(SubmarinePos(0, 0))((acc, cmd) => acc.execute(cmd))

  /** * Compute the position of the submarine after applying series of commands. (part2 of the puzzle)
    */
  def computePosition2(inputs: Seq[String]): Int = {
    val commands = inputs.flatMap(parseCommand)
    val pos      = doComputePosition2(commands)
    pos.depth * pos.horizontal
  }

  private def doComputePosition2(commands: Seq[SubmarineCmd]): SubmarinePos =
    commands.foldLeft(SubmarinePos(0, 0))((acc, cmd) => acc.execute2(cmd))

  private val forwardRegex = """forward (\d+)""".r
  private val downRegex    = """down (\d+)""".r
  private val upRegex      = """up (\d+)""".r
  private def parseCommand(input: String): Option[SubmarineCmd] = input match {
    case forwardRegex(steps) => Some(ForwardCmd(steps.toInt))
    case downRegex(steps)    => Some(DownCmd(steps.toInt))
    case upRegex(steps)      => Some(UpCmd(steps.toInt))
    case _                   => None
  }

}
