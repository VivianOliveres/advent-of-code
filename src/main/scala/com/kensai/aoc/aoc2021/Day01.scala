package com.kensai.aoc.aoc2021

import scala.collection.immutable.Seq

object Day01 {

  /**
    * Compute the number of consecutive increases.
    */
  def compute(inputs: Seq[Long]): Long = {
    val zero: (Long, Long) = (0L, inputs.head)
    val (result, _) = inputs.tail.foldLeft(zero) {(acc, value) =>
      acc match {
        case (count, previous) if (value > previous) => (count + 1, value)
        case (count, _) => (count, value)
      }
    }
    result
  }

  /**
    * Accumulator class that keeps the number of previous increases and the previous sliding windows.
    */
  case class Acc(count: Long, old: Seq[Long], mid: Seq[Long], young: Seq[Long]) {

    /**
      * Build a new Accumulator with a new value:
      * 1) Update the count of increased window
      * 2) Slide windows
      * 3) Update windows with new value
      */
    def add(value: Long): Acc = {
      if (old.size < 3)
        Acc(count, mid :+ value, young :+ value, Seq(value))
      else  {
        val newResult = if (old.sum < mid.sum + value) count + 1 else count
        Acc(newResult, mid :+ value, young :+ value, Seq(value))
      }
    }
  }

  /**
    * Compute the number of consecutive increases based on the sum of 3 last elements.
    */
  def compute2(inputs: Seq[Long]): Long = {
    val zero: Acc = Acc(0L, Seq(), Seq(), Seq())
    val acc: Acc = inputs.foldLeft(zero) { (left, value) =>
      left.add(value)
    }
    acc.count
  }

}
