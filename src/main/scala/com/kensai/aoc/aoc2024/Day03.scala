package com.kensai.aoc.aoc2024

import scala.annotation.tailrec

object Day03 {

  sealed trait Command
  case class Mul(x: Int, y: Int) extends Command
  case object Do extends Command
  case object Dont extends Command

  private val mulRegex = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r

  def parseMul(row: String): Option[Command] = row match {
    case mulRegex(x, y) => Some(Mul(x.toInt, y.toInt))
    case _ => None
  }

  def parseAllMul(input: String): Seq[Mul] =
    mulRegex
      .findAllMatchIn(input)
      .map(m => Mul(m.group(1).toInt, m.group(2).toInt))
      .toSeq

  def executeAll(commands: Seq[Mul]): Int =
    commands.map(c => c.x * c.y).sum

  def parseAllCommands(input: String): Seq[Command] =
    doParse(input, Seq.empty[Command])

  @tailrec
  def doParse(input: String, acc: Seq[Command]): Seq[Command] = {
    if (input.isEmpty)
      acc
    else if(input.head != 'd' && input.head != 'm') // Fast Skip
      doParse(input.tail, acc)
    else if (input.length >= 4 && input.startsWith("do()"))
      doParse(input.substring(4), acc :+ Do)
    else if (input.length >= 7 && input.startsWith("don't()"))
      doParse(input.substring(7), acc :+ Dont)
    else if (input.length >= 4 && input.startsWith("mul(")) {
      // 12 is mul(x,y) where x and y can be 3 digits
      val row = input.substring(0, math.min(12, input.length))
      val closeIndex = row.indexOf(")")
      if (closeIndex < 0) // No close parenthesis => skip
        doParse(input.substring(4), acc)
      else {
        val maybeMul = row.substring(0, closeIndex + 1) match {
          case mulRegex(x, y) => Some(Mul(x.toInt, y.toInt))
          case _ => None
        }
        doParse(input.substring(4), acc ++ maybeMul.toSeq)
      }
    } else
      doParse(input.tail, acc)
  }

  def executeAllCommands(commands: Seq[Command]): Int =
    doExecuteAllCommands(commands = commands, ignoreMul = false, acc = 0)

  @tailrec
  private def doExecuteAllCommands(commands: Seq[Command], ignoreMul: Boolean, acc: Int): Int = {
    if (commands.isEmpty)
      acc
    else {
      commands.head match {
        case Do =>
          doExecuteAllCommands(commands = commands.tail, ignoreMul = false, acc = acc)
        case Dont =>
          doExecuteAllCommands(commands = commands.tail, ignoreMul = true, acc = acc)
        case Mul(_, _) if ignoreMul =>
          doExecuteAllCommands(commands = commands.tail, ignoreMul = ignoreMul, acc = acc)
        case Mul(x, y) =>
          doExecuteAllCommands(commands = commands.tail, ignoreMul = ignoreMul, acc = acc + x * y)
      }
    }
  }

}
