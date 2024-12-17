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
        }else {
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

  // TODO: part2

//  def outputItself(input: Day17Input): Long = {
//    findMatchingOutput(input, input.program)
//  }
//
//  def findMatchingOutput(input: Day17Input, target: Seq[Long]): Long = {
//    var aStart = if (target.size == 1)
//      0L
//    else
//      findMatchingOutput(input, target.tail) >> 3
//
//    println(s"$aStart target[$target]")
//    var newInput = input.copy(a = aStart)
//    var result = executeInput(newInput)
//    while (result.output != target) {
//      newInput = input.copy(a = aStart)
//      result = executeInput(newInput)
//      aStart = aStart + 1
////      println(s"$aStart result[${result.output}]")
//      if (result.output.size > target.size)
//        throw new IllegalArgumentException(s"Invalid sizes: output[${result.output}] target[${target}]")
//    }
//
//    println(s"$target => $aStart")
//    aStart
//  }

}
