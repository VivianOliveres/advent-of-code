package com.kensai.aoc.lib

import scala.annotation.tailrec

object Pairs {

  def generateAllPairs[T](elements: Seq[T]): Seq[(T, T)] =
    generateOrderedPairs(elements).flatMap { case (l, r) => Seq((l, r), (r, l)) }

  def generateOrderedPairs[T](elements: Seq[T]): Seq[(T, T)] =
    doGenerateAllPairs(elements, Seq.empty[(T, T)])

  @tailrec
  private def doGenerateAllPairs[T](elements: Seq[T], acc: Seq[(T, T)]): Seq[(T, T)] = elements match {
    case Nil      => acc
    case _ :: Nil => acc
    case head :: tail =>
      val ro  = tail.map((head, _))
      val roo = acc ++ ro
      doGenerateAllPairs(elements.tail, roo)
    case other => throw new IllegalArgumentException(s"Should not happen: $other")
  }

}
