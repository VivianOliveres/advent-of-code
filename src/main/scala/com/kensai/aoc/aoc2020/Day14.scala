package com.kensai.aoc.aoc2020

import com.kensai.aoc.aoc2020.Day14.Mask.valueToBitSet

import scala.collection.immutable.BitSet

object Day14 {

  case class Mask(maskStr: String, positiveMask: BitSet, negativeMask: BitSet) {
    private def positiveBitSet(value: Long): BitSet =
      valueToBitSet(value.toBinaryString, keepPositive = true)

    private def compute(valueToWrite: Long): BitSet =
      (positiveBitSet(valueToWrite) | positiveMask) &~ negativeMask

    def computeLong(valueToWrite: Long): Long =
      compute(valueToWrite).toBitMask.head

    /** For each BitSet in `acc`, generate two new BitSet with 1 and 0 at `floatingIndex`.
      */
    private def generateFloatingBitSets(
        acc: List[BitSet],
        floatingIndex: Int
    ): List[BitSet] =
      acc.flatMap(bitset =>
        List(bitset + floatingIndex, bitset - floatingIndex)
      )

    def computeAddresses(valueToWrite: Long): Set[Long] = {
      // Start by applying the or BitMask
      val orResult = positiveBitSet(valueToWrite) | positiveMask
      val xIndexes = maskStr.reverse.zipWithIndex
        .filter(_._1 == 'X')
        .map(_._2)
      xIndexes
        .foldLeft(List(orResult)) { generateFloatingBitSets }
        .map(_.toBitMask.head) // Convert to long
        .toSet
    }
  }

  object Mask {
    def valueToBitSet(input: String, keepPositive: Boolean): BitSet =
      input.reverse.zipWithIndex
        .filter(i => if (keepPositive) i._1 == '1' else i._1 == '0')
        .foldLeft(BitSet.empty) { case (l, r) => l + r._2 }

    def apply(input: String): Mask = {
      new Mask(
        maskStr = input,
        positiveMask = valueToBitSet(input, keepPositive = true),
        negativeMask = valueToBitSet(input, keepPositive = false)
      )
    }
  }

  case class Instruction(memoryAddress: Long, valueToApply: Long)

  case class Input(mask: Mask, instructions: List[Instruction])

  private val memRegex = """mem\[(\d+)] = (\d+)""".r

  def parse(inputs: String): List[Input] =
    inputs
      .split("mask = ")
      .toList
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(other => {
        val rows = other.split("\n")
        val mask = Mask(rows.head)
        val instructions =
          rows.tail.map {
            case memRegex(memoryAddress, valueToApply) =>
              Instruction(memoryAddress.toLong, valueToApply.toLong)
            case something =>
              throw new RuntimeException(s"Unknown instruction [$something]")
          }.toList
        Input(mask, instructions)
      })

  def computePart1(input: String): Long = {
    val inputs = parse(input)
    inputs
      .flatMap(doComputePart1)
      .groupBy(_._1)
      .map { case (_, counts) => counts.last }
      .values
      .sum
  }

  def doComputePart1(input: Input): Map[Long, Long] =
    input.instructions
      .groupBy(_.memoryAddress)
      .map { case (memoryAddress, instructions) =>
        (memoryAddress, instructions.last)
      }
      .map { case (memoryAddress, instr) =>
        (memoryAddress, input.mask.computeLong(instr.valueToApply))
      }

  def doComputePart2(input: Input): Map[Long, Long] =
    input.instructions
      .flatMap(instr =>
        input.mask
          .computeAddresses(instr.memoryAddress)
          .map((_, instr.valueToApply))
      )
      .groupBy(_._1)
      .map { case (memoryAddress, values) =>
        memoryAddress -> values.map(_._2).last
      }

  def computePart2(inputs: List[Input]): Long =
    inputs
      .flatMap(doComputePart2)
      .groupBy(_._1)
      .map { case (_, counts) => counts.last }
      .values
      .sum

  def computePart2(input: String): Long = {
    val inputs = parse(input)
    computePart2(inputs)
  }
}
