package com.kensai.aoc.aoc2022

import scala.util.parsing.combinator._

object Day13 extends RegexParsers {

  sealed trait Packet
  case class ValuePacket(value: Int) extends Packet
  case class ListPacket(packets: Seq[Packet]) extends Packet {
    def tail: ListPacket =
      ListPacket(packets.tail)
  }

  case class InputDay13(index: Int, left: Packet, right: Packet)

  def parse(lines: Seq[String]): Seq[InputDay13] =
    lines
      .grouped(3)
      .zipWithIndex
      .map { case (rows, index) =>
        val left  = parseLine(rows.head)
        val right = parseLine(rows(1))
        InputDay13(index + 1, left, right)
      }
      .toSeq

  def parsePacket(s: String): Packet = {

    def packetNode: Parser[Packet] = (
      "\\d+".r ^^ (value => ValuePacket(value.toInt))
        | "[" ~> repsep(packetNode, ",") <~ "]" ^^ ListPacket.apply
    )

    parseAll(packetNode, s).get
  }

  def parseLine(input: String): Packet =
    input.linesIterator.map(parsePacket).next()

  def areInRightOrder(input: InputDay13): Boolean =
    doCompare(input.left, input.right).get

  private def doCompare(left: Packet, right: Packet): Option[Boolean] = (left, right) match {
    case (ValuePacket(leftInt), ValuePacket(rightInt)) =>
      if (leftInt < rightInt) Some(true) else if (leftInt == rightInt) None else Some(false)
    case (ListPacket(_), ValuePacket(rightInt)) => doCompare(left, ListPacket(Seq(ValuePacket(rightInt))))
    case (ValuePacket(leftInt), ListPacket(_))  => doCompare(ListPacket(Seq(ValuePacket(leftInt))), right)
    case (ListPacket(Nil), ListPacket(Nil))     => None
    case (ListPacket(Nil), ListPacket(_))       => Some(true)
    case (ListPacket(_), ListPacket(Nil))       => Some(false)
    case (leftPacket @ ListPacket(leftList), rightPacket @ ListPacket(rightList)) =>
      val maybeResult = doCompare(leftList.head, rightList.head)
      if (maybeResult.isEmpty)
        doCompare(leftPacket.tail, rightPacket.tail)
      else
        maybeResult
  }

  def countRightOrders(inputs: Seq[InputDay13]): Int =
    inputs.filter(areInRightOrder).map(_.index).sum

  def dividerPackets(inputs: Seq[InputDay13]): Int = {
    val divider2   = ListPacket(Seq(ListPacket(Seq(ValuePacket(2)))))
    val divider6   = ListPacket(Seq(ListPacket(Seq(ValuePacket(6)))))
    val allPackets = inputs.flatMap(input => Seq(input.left, input.right)) :+ divider2 :+ divider6

    val sortedPackets = allPackets.sorted(new PacketOrdering)

    val divider2Index = sortedPackets.indexOf(divider2) + 1
    val divider6Index = sortedPackets.indexOf(divider6) + 1
    divider2Index * divider6Index
  }

  private class PacketOrdering extends Ordering[Packet] {
    override def compare(x: Packet, y: Packet): Int = {
      val maybeResult = doCompare(x, y)
      maybeResult match {
        case None       => 0
        case Some(true) => -1
        case _          => 1
      }
    }
  }
}
