package com.kensai.aoc.aoc2021

import scala.annotation.tailrec
import scala.collection.Seq

object Day16 {

  type Bits = Seq[Boolean]

  sealed trait SubPacket {
    def eval: Long
  }
  case class LiteralPacket(value: Long) extends SubPacket {
    override def eval: Long = value
  }
  case class OperatorPacket(
      typeId: Int,
      subMessages: Seq[Message])
      extends SubPacket {
    override def eval: Long = typeId match {
      case 0 => subMessages.map(_.eval).sum
      case 1 => subMessages.map(_.eval).product
      case 2 => subMessages.map(_.eval).min
      case 3 => subMessages.map(_.eval).max
      case 5 =>
        val (first :: second :: _) = subMessages.map(_.eval)
        if (first > second) 1L else 0L
      case 6 =>
        val (first :: second :: _) = subMessages.map(_.eval)
        if (first < second) 1L else 0L
      case 7 =>
        val (first :: second :: _) = subMessages.map(_.eval)
        if (first == second) 1L else 0L
      case _ =>
        throw new IllegalArgumentException(
          s"Invalid typeId[$typeId] for [$this]"
        )
    }
  }

  case class Message(packetVersion: Int, subPacket: SubPacket) {
    def eval: Long = subPacket.eval
  }

  def computeSumVersionNumbers(message: Message): Int =
    message match {
      case Message(packetVersion, LiteralPacket(_)) => packetVersion
      case Message(packetVersion, OperatorPacket(_, subMessages)) =>
        packetVersion + subMessages.map(computeSumVersionNumbers).sum
    }

  def toBits(value: String): Bits =
    value.map {
      case '0' => false
      case '1' => true
      case _ =>
        throw new IllegalArgumentException(s"Invalid bit value for [$value]")
    }

  def bitsToString(bits: Bits): String =
    bits.map(b => if (b) '1' else '0').mkString

  def hexToBin(input: String): Bits =
    input.trim.flatMap {
      case '0' => Seq(false, false, false, false)
      case '1' => Seq(false, false, false, true)
      case '2' => Seq(false, false, true, false)
      case '3' => Seq(false, false, true, true)
      case '4' => Seq(false, true, false, false)
      case '5' => Seq(false, true, false, true)
      case '6' => Seq(false, true, true, false)
      case '7' => Seq(false, true, true, true)
      case '8' => Seq(true, false, false, false)
      case '9' => Seq(true, false, false, true)
      case 'A' => Seq(true, false, true, false)
      case 'B' => Seq(true, false, true, true)
      case 'C' => Seq(true, true, false, false)
      case 'D' => Seq(true, true, false, true)
      case 'E' => Seq(true, true, true, false)
      case 'F' => Seq(true, true, true, true)
    }

  def parse(input: Bits): Message =
    doParse(input)._1

  private def doParse(
      remainingInput: Bits
    ): (Message, Bits) = {
    val (packetVersionBits, remainingBits) = remainingInput.splitAt(3)
    val packetVersion                      = parseBinString(packetVersionBits).toInt
    val (typeIdBits, otherRemainingBits)   = remainingBits.splitAt(3)
    val typeId                             = parseBinString(typeIdBits).toInt
    typeId match {
      case 4 =>
        val (value, finalRemaining) = doParseLiteral(otherRemainingBits, Seq())
        (Message(packetVersion, LiteralPacket(value)), finalRemaining)
      case _ =>
        val (operatorPacket, finalRemaining) =
          doParseOperator(typeId, otherRemainingBits)
        (Message(packetVersion, operatorPacket), finalRemaining)
    }
  }

  @tailrec
  private def doParseLiteral(
      remainingInput: Bits,
      binSeq: Bits
    ): (Long, Bits) =
    if (remainingInput.head) {
      val (firstValue, otherBits) = remainingInput.tail.splitAt(4)
      val value                   = binSeq ++ firstValue
      doParseLiteral(otherBits, value)
    } else {
      val (firstValue, finalRemaining) = remainingInput.tail.splitAt(4)
      val bits                         = binSeq ++ firstValue
      (parseBinString(bits), finalRemaining)
    }

  private def parseBinString(bits: Bits): Long =
    bits.foldLeft(0L) { case (acc, bit) => (acc << 1) | (if (bit) 1 else 0) }

  private def doParseOperator(
      typeId: Int,
      remainingInput: Bits
    ): (OperatorPacket, Bits) =
    if (remainingInput.head) {
      val (numberOfSubMessagesBits, remainingBits) =
        remainingInput.tail.splitAt(11)
      val numberOfSubMessages = parseBinString(numberOfSubMessagesBits).toInt
      val (subMessages, finalRemaining) =
        doParseMessageNumbers(remainingBits, numberOfSubMessages, Seq())
      (OperatorPacket(typeId, subMessages), finalRemaining)
    } else {
      val (valueBits, remainingBits) = remainingInput.tail.splitAt(15)
      val sizeMessages               = parseBinString(valueBits).toInt
      val (msgsBits, remaining)      = remainingBits.splitAt(sizeMessages)
      val (messages, _)              = doParseMessageSize(msgsBits, Seq())
      (OperatorPacket(typeId, messages), remaining)
    }

  @tailrec
  private def doParseMessageNumbers(
      bits: Bits,
      messageCounts: Int,
      acc: Seq[Message]
    ): (Seq[Message], Bits) =
    if (messageCounts == 0)
      (acc, bits)
    else {
      val (msg, remainingBits) = doParse(bits)
      doParseMessageNumbers(remainingBits, messageCounts - 1, acc :+ msg)
    }

  @tailrec
  private def doParseMessageSize(
      bits: Bits,
      acc: Seq[Message]
    ): (Seq[Message], Bits) =
    bits match {
      case Nil => (acc, Nil)
      case _ =>
        val (msg, remaining) = doParse(bits)
        doParseMessageSize(remaining, acc :+ msg)
    }

}
