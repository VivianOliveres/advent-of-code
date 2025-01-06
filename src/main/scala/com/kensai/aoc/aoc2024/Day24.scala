package com.kensai.aoc.aoc2024

import scala.annotation.tailrec

object Day24 {

  sealed trait GateFunction {
    def leftInput: String
    def rightInput: String
    def compute(input1: Boolean, input2: Boolean): Boolean
  }
  case class AndGate(leftInput: String, rightInput: String) extends GateFunction {
    override def compute(input1: Boolean, input2: Boolean): Boolean = input1 && input2
  }
  case class OrGate(leftInput: String, rightInput: String) extends GateFunction {
    override def compute(input1: Boolean, input2: Boolean): Boolean = input1 || input2
  }
  case class XorGate(leftInput: String, rightInput: String) extends GateFunction {
    override def compute(input1: Boolean, input2: Boolean): Boolean = input1 != input2
  }

  case class Day24Input(state: Map[String, Boolean], rest: Map[String, GateFunction])

  private val andRegex = "(\\w+)\\s+AND\\s+(\\w+)\\s+->\\s+(\\w+)".r
  private val orRegex  = "(\\w+)\\s+OR\\s+(\\w+)\\s+->\\s+(\\w+)".r
  private val xorRegex = "(\\w+)\\s+XOR\\s+(\\w+)\\s+->\\s+(\\w+)".r

  def parse(input: String): Day24Input = {
    val splitParts = input.split("\n\n")
    val stateRows = splitParts.head
      .split("\n")
      .toSeq
      .map { row =>
        val splitRow  = row.split(": ")
        val wireName  = splitRow.head
        val wireValue = splitRow(1) == "1"
        (wireName, wireValue)
      }
      .toMap

    val gates = splitParts(1)
      .split("\n")
      .map {
        case andRegex(left, right, result) => (result, AndGate(left, right))
        case orRegex(left, right, result)  => (result, OrGate(left, right))
        case xorRegex(left, right, result) => (result, XorGate(left, right))
        case row                           => throw new IllegalArgumentException(s"Invalid row[$row]")
      }
      .toMap

    Day24Input(stateRows, gates)
  }

  def computeRegistry(input: Day24Input): Long = {
    val allStates  = doComputeRegistry(input)
    val zRegisters = allStates.keys.filter(_.startsWith("z")).toSeq.sorted
    zRegisters.indices.foldLeft(0L) { (acc, i) =>
      val zName  = zRegisters(i)
      val zValue = if (allStates(zName)) 1 else 0
      acc + zValue * math.pow(2.0, i.toDouble).toLong
    }
  }

  @tailrec
  private def doComputeRegistry(input: Day24Input): Map[String, Boolean] =
    if (input.rest.isEmpty)
      input.state
    else {
      val executed = input.rest.flatMap { case (name, fn) =>
        if (input.state.contains(fn.leftInput) && input.state.contains(fn.rightInput)) {
          Some((name, fn.compute(input.state(fn.leftInput), input.state(fn.rightInput))))
        } else {
          None
        }
      }
      if (executed.isEmpty)
        input.state
      else {
        val newState = input.state ++ executed
        val newRest  = input.rest -- executed.keys
        val newInput = Day24Input(newState, newRest)
        doComputeRegistry(newInput)
      }

    }

  /**
   * Compute the registries and returns (x, y, z, x+y)
   */
  def compute(input: Day24Input): (Long, Long, Long, Long) = {
    val allStates  = doComputeRegistry(input)

    val zRegisters = allStates.keys.filter(_.startsWith("z")).toSeq.sorted
    val z = zRegisters.indices.foldLeft(0L) { (acc, i) =>
      val zName  = zRegisters(i)
      val zValue = if (allStates(zName)) 1 else 0
      acc + zValue * math.pow(2.0, i.toDouble).toLong
    }

    val xRegisters = allStates.keys.filter(_.startsWith("x")).toSeq.sorted
    val x = xRegisters.indices.foldLeft(0L) { (acc, i) =>
      val zName  = xRegisters(i)
      val zValue = if (allStates(zName)) 1 else 0
      acc + zValue * math.pow(2.0, i.toDouble).toLong
    }

    val yRegisters = allStates.keys.filter(_.startsWith("y")).toSeq.sorted
    val y = yRegisters.indices.foldLeft(0L) { (acc, i) =>
      val zName  = yRegisters(i)
      val zValue = if (allStates(zName)) 1 else 0
      acc + zValue * math.pow(2.0, i.toDouble).toLong
    }

    (x, y, z, x + y)
  }

  // Return the bits that differs between two numbers
  def diff(num1: Long, num2: Long): Seq[Int] = {
    val xorResult = num1 ^ num2
    (0 until 64).filter(i => (xorResult & (1L << i)) != 0)
  }

  // Part 2 is very hard to code.
  // It is mainly a manual research problem.

  // Also, this machine is a bit adder: https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder
  // So, for each bits, we are looking for a list of gates like: (only x, y, z, Cin and Cout are valid names, others are for the exemple)
  // x XOR y -> a
  // a XOR Cin -> z
  // a AND Cin -> b
  // x AND y -> c
  // b OR c -> Cout

  // So first observation is that Z is the output of a XOR
  // If we parse all z that are not a XOR outputs we find:
  // input.rest.filter{
  //   case (output, XorGate(_, _)) if output.head == 'z' => false
  //   case _ => true
  // }.toSeq.sortBy(_._1).foreach(println)
  // => (z07,AndGate(gnj,scw))
  // => (z23,OrGate(jwb,hjp))
  // => (z27,AndGate(x27,y27))
  // So z07, z23 and z27 should be exchanged with their correct XOR(_, _) to produce the correct output
  // After investigating we have to swap:
  //    z07 <=> shj
  //    z23 <=> pfn
  //    z27 <=> kcd

  // Second we could check the first bit that differs between z and x+y (after swap previous):
  // val (_, _, z, xy) = compute(input)
  // val bitsThatDiffers = Numbers.bitDifferences(z, xy)
  // => Vector(16, 17, 18, 19, 20, 27, 28, 29, 30)
  // So we investigate around z16 and we found that we need to switch tpk and wkb
  //    tpk <=> wkb

  // Then solution becomes:
  // val gateDiffs = Seq("z27", "kcd", "z23", "pfn", "tpk", "wkb", "z07", "shj")
  // val result = gateDiffs.sorted.mkString(",")
  // println(result)
  // => kcd,pfn,shj,tpk,wkb,z07,z23,z27

}
