package com.kensai.aoc.aoc2021

import scala.annotation.tailrec
import cats.implicits._

object Day03 {

  def computePowerConsumption(inputs: Seq[String]): Int = {
    val parsedInputs = parse(inputs)
    val sizeSeq = inputs.head.length
    val countByIndex = (0 until sizeSeq).map(index => (index, countBits(index, parsedInputs)))
    val (gamma, epsilon) = countByIndex.map{case (index, (ones, zeros))  =>
      val bit = ones >= zeros
      val binaryIndex = sizeSeq - index - 1
      (bitToInt(bit, binaryIndex), bitToInt(!bit, binaryIndex))
    }.foldLeft((0, 0)){case (l, r) => l.combine(r)}

    gamma * epsilon
  }

  private def parse(inputs: Seq[String]): Seq[Seq[Boolean]] =
    inputs
      .filterNot(_.isEmpty)
      .map(_.map(_ == '1'))

  private def countBits(index: Int, inputs: Seq[Seq[Boolean]]): (Int, Int) = {
    inputs.map(input => input(index)).count(_ == true)
    val (positives, negatives) = inputs.map(input => input(index)).partition(_ == true)
    (positives.size, negatives.size)
  }

  private def bitToInt(bit: Boolean, binaryIndex: Int): Int =
    bitToInt(bit) * math.pow(2.0, binaryIndex.toDouble).toInt

  private def bitToInt(value: Boolean): Int =
    if (value) 1 else 0

  def computeLifeSupportRating(inputs: Seq[String]): Long = {
    val parsedInputs = parse(inputs)
    val oxygenGeneratorRating = computeOxygenGeneratorRating(0, parsedInputs)
    val co2ScrubberRating = computeCo2ScrubberRating(0, parsedInputs)
    oxygenGeneratorRating * co2ScrubberRating
  }

  @tailrec
  def computeOxygenGeneratorRating(index: Int, inputs: Seq[Seq[Boolean]]): Long = {
    if (inputs.size == 1) bitsToLong(inputs.head)
    else {
      val (positives, negatives) = countBits(index, inputs)
      val mostCommonBit = positives >= negatives
      val filteredInputs = inputs.filter(input => input(index) == mostCommonBit)
      computeOxygenGeneratorRating(index + 1, filteredInputs)
    }
  }

  private def bitsToLong(bits: Seq[Boolean]): Long = bits.zipWithIndex.foldLeft(0L) {
    case (acc, (bit, index)) => acc + bitToInt(bit) * math.pow(2.0, bits.size - index - 1.0).toInt
  }

  @tailrec
  def computeCo2ScrubberRating(index: Int, inputs: Seq[Seq[Boolean]]): Long = {
    if (inputs.size == 1) bitsToLong(inputs.head)
    else {
      val (positives, negatives) = countBits(index, inputs)
      val leastCommonBit = !(positives >= negatives)
      val filteredInputs = inputs.filter(input => input(index) == leastCommonBit)
      computeCo2ScrubberRating(index + 1, filteredInputs)
    }
  }

}
