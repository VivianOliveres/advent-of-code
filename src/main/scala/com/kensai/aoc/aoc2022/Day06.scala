package com.kensai.aoc.aoc2022

import scala.annotation.tailrec

object Day06 {

  def startPacketMarker(input: String): Int =
    doStartMarker(3, input, Seq(), 0)

  def startMessageMarker(input: String): Int =
    doStartMarker(13, input, Seq(), 0)

  @tailrec
  private def doStartMarker(bufferSize: Int, remaining: Seq[Char], buffer: Seq[Char], currentIndex: Int): Int = {
    val head = remaining.head
    if (buffer.contains(head)) {
      val newBuffer = buffer.dropWhile(_ != head).tail :+ head
      doStartMarker(bufferSize, remaining.tail, newBuffer, currentIndex + 1)
    } else if (buffer.size < bufferSize)
      doStartMarker(bufferSize, remaining.tail, buffer :+ head, currentIndex + 1)
    else
      currentIndex + 1
  }
}
