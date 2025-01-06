package com.kensai.aoc.lib

object Numbers {

  /**
   * Return the lists of bits that differs between these 2 numbers.
   * For instance, bitDifferences(1,3) will return Seq(1) as 1=01 and 3=11 so only bit 1 changes.
   */
  def bitDifferences(num1: Long, num2: Long): Seq[Int] = {
    val xorResult = num1 ^ num2
    (0 until 64).filter(i => (xorResult & (1L << i)) != 0)
  }

}
