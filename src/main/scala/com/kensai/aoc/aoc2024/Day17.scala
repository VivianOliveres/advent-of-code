package com.kensai.aoc.aoc2024

import scala.annotation.tailrec

object Day17 {

  case class Day17Input(a: Long, b: Long, c: Long, program: Seq[Long], output: Seq[Long], operationPointer: Int) {

    def comboOperand(operand: Long): Long = operand match {
      case 0 => operand
      case 1 => operand
      case 2 => operand
      case 3 => operand
      case 4 => a
      case 5 => b
      case 6 => c
      case _ => throw new IllegalArgumentException(s"Invalid operand [$operand]")
    }
  }

  def parse(rows: Seq[String]): Day17Input = {
    val a = rows.head.split(" ")(2).toLong
    val b = rows(1).split(" ")(2).toLong
    val c = rows(2).split(" ")(2).toLong
    val program = rows(4).split(" ")(1).split(",").toSeq.map(_.toLong)
    Day17Input(a, b, c, program, Seq(), 0)
  }

  def executeInput(input: Day17Input): Day17Input = {
    doExecute(input)
  }

  @tailrec
  def doExecute(input: Day17Input): Day17Input = {
    if (input.operationPointer > input.program.size - 1)
      input
    else {
      val opcode = input.program(input.operationPointer)
      val operand = input.program(input.operationPointer + 1)
      val (a, b, c, output, pointer) = doExecute(opcode, operand, input)
      val result = input.copy(a = a, b = b, c = c, output = output, operationPointer = pointer)
      doExecute(result)
    }
  }

  def doExecute(opcode: Long, operand: Long, input: Day17Input): (Long, Long, Long, Seq[Long], Int) = {
    opcode match {
      case 0 =>
        val comboOperand = input.comboOperand(operand)
        val newA = input.a / math.pow(2.0, comboOperand.toDouble).toLong
        (newA, input.b, input.c, input.output, input.operationPointer + 2)
      case 1 =>
        val newB = input.b ^ operand
        (input.a, newB, input.c, input.output, input.operationPointer + 2)
      case 2 =>
        val comboOperand = input.comboOperand(operand)
        val newB = comboOperand % 8
        (input.a, newB, input.c, input.output, input.operationPointer + 2)
      case 3 =>
        if (input.a == 0) {
          (input.a, input.b, input.c, input.output, input.operationPointer + 2)
        } else {
          (input.a, input.b, input.c, input.output, operand.toInt)
        }
      case 4 =>
        val newB = input.b ^ input.c
        (input.a, newB, input.c, input.output, input.operationPointer + 2)
      case 5 =>
        val comboOperand = input.comboOperand(operand)
        val newOutput = comboOperand % 8
        (input.a, input.b, input.c, input.output :+ newOutput, input.operationPointer + 2)
      case 6 =>
        val comboOperand = input.comboOperand(operand)
        val newB = input.a / math.pow(2.0, comboOperand.toDouble).toInt
        (input.a, newB, input.c, input.output, input.operationPointer + 2)
      case 7 =>
        val comboOperand = input.comboOperand(operand)
        val newC = input.a / math.pow(2.0, comboOperand.toDouble).toInt
        (input.a, input.b, newC, input.output, input.operationPointer + 2)
      case _ =>
        throw new IllegalArgumentException(s"Invalid opcode [$opcode] for operand [$operand]")
    }
  }

  def solvePart2(input: Day17Input): Long = {
    doSolve(input, Seq(3L))
  }

  @tailrec
  private def doSolve(baseInput: Day17Input, todo: Seq[Long]): Long = {
    val a = todo.head

    // Generate next As by multiplying by 8 (ie 3 bytes moves)
    // then generates all possible bytes for strong byte (ie (0 to 7).map(n => ... | n)
    val newAs = (0 to 7).map(n => (a << 3) | n)

    // Select all A that produces the correct output (so far)
    val allWorkingAs = newAs
      .map(newA => (newA, baseInput.copy(a = newA)))
      .map{case (newA, newInput) => (newA, executeInput(newInput))}
      .filter{case (_, newInput) => newInput.output == baseInput.program.takeRight(newInput.output.size)}

    if (allWorkingAs.isEmpty)
      doSolve(baseInput, todo.tail)
    else {
      // Return first/smaller solution
      val maybeSolution = allWorkingAs.find(i => i._2.output == baseInput.program)
      if (maybeSolution.isDefined)
        maybeSolution.get._1
      else {
        val nextTod = allWorkingAs.map(_._1)
        doSolve(baseInput, todo.tail ++ nextTod)
      }
    }
  }
}