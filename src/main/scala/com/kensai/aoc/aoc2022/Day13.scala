package com.kensai.aoc.aoc2022

import scala.annotation.tailrec

object Day13 {

  sealed trait Packet
  case class ValuePacket(value: Int) extends Packet
  case class ListPacket(packets: Seq[Packet]) extends Packet {
    def tail: ListPacket =
      ListPacket(packets.tail)
  }

  case class InputDay13(index: Int, left: ListPacket, right: ListPacket)

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

  def parseLine(line: String): ListPacket =
    doParse(line)._1
      .asInstanceOf[ListPacket]

  private def doParse(remaining: String): (Packet, String) =
    if (remaining.isEmpty)
      (ListPacket(Seq()), remaining)
    else
      remaining.head match {
        case '[' =>
          val endIndex             = lastIndex(remaining.tail, 0, 1)
          val substringToConsider  = remaining.substring(1, endIndex)
          var results: Seq[Packet] = Seq()
          var remainings           = substringToConsider
          while (remainings.nonEmpty) {
            val (result, tmpRemaining) = doParse(remainings)
            results = results :+ result
            if (tmpRemaining.startsWith(","))
              remainings = tmpRemaining.tail
            else
              remainings = tmpRemaining
          }

          val newRemaining = remaining.substring(endIndex + 1)
          (ListPacket(results), newRemaining)
        case _ =>
          val index = remaining.indexOf(",")
          if (index < 0)
            (ValuePacket(remaining.toInt), "")
          else {
            val split  = remaining.substring(0, index)
            val result = ValuePacket(split.toInt)
            (result, remaining.substring(index + 1))
          }
      }

  @tailrec
  private def lastIndex(remaining: String, countOpenBraces: Int, currentIndex: Int): Int = remaining.head match {
    case ']' if countOpenBraces == 0 => currentIndex
    case ']'                         => lastIndex(remaining.tail, countOpenBraces - 1, currentIndex + 1)
    case '['                         => lastIndex(remaining.tail, countOpenBraces + 1, currentIndex + 1)
    case _                           => lastIndex(remaining.tail, countOpenBraces, currentIndex + 1)
  }

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
