package com.kensai.aoc.aoc2024

object Day04 {

  def countAll(input: Seq[String], counter: (Seq[String], Int, Int) => Int): Int = {
    val maxX = input.head.length
    val maxY = input.size
    val countsForEachPosition = for {
      x <- 0 until maxX
      y <- 0 until maxY
    } yield counter(input, x, y)
    countsForEachPosition.sum
  }

  def countXmasAt(input: Seq[String], x: Int, y: Int): Int = {
    val maxX = input.head.length - 1
    val maxY = input.size - 1
    if (input(y).charAt(x) != 'X') // Check that current position is X
      0
    else {
      // Check XMAS at right
      val right =
        if (maxX >= x + 3 && input(y).charAt(x + 1) == 'M' && input(y).charAt(x + 2) == 'A' && input(y).charAt(x + 3) == 'S')
          1
        else
          0

      // Check XMAS at left
      val left =
        if (x - 3 >= 0 && input(y).charAt(x - 1) == 'M' && input(y).charAt(x - 2) == 'A' && input(y).charAt(x - 3) == 'S')
          1
        else
          0

      // Check XMAS on top
      val top =
        if (y + 3 <= maxY && input(y + 1).charAt(x) == 'M' && input(y + 2).charAt(x) == 'A' && input(y + 3).charAt(x) == 'S') 1 else 0

      // Check XMAS on bottom
      val bottom =
        if (y - 3 >= 0 && input(y - 1).charAt(x) == 'M' && input(y - 2).charAt(x) == 'A' && input(y - 3).charAt(x) == 'S') 1 else 0

      // Check XMAS on diagonal top left
      val topLeft =
        if (
          y + 3 <= maxY && x - 3 >= 0 && input(y + 1).charAt(x - 1) == 'M' && input(y + 2)
            .charAt(x - 2) == 'A' && input(y + 3).charAt(x - 3) == 'S'
        ) 1
        else 0

      // Check XMAS on diagonal top right
      val topRight =
        if (
          y + 3 <= maxY && maxX >= x + 3 && input(y + 1).charAt(x + 1) == 'M' && input(y + 2)
            .charAt(x + 2) == 'A' && input(y + 3).charAt(x + 3) == 'S'
        ) 1
        else 0

      // Check XMAS on diagonal bottom left
      val bottomLeft =
        if (
          y - 3 >= 0 && x - 3 >= 0 && input(y - 1).charAt(x - 1) == 'M' && input(y - 2)
            .charAt(x - 2) == 'A' && input(y - 3).charAt(x - 3) == 'S'
        ) 1
        else 0

      // Check XMAS on diagonal top right
      val bottomRight =
        if (
          y - 3 >= 0 && maxX >= x + 3 && input(y - 1).charAt(x + 1) == 'M' && input(y - 2)
            .charAt(x + 2) == 'A' && input(y - 3).charAt(x + 3) == 'S'
        ) 1
        else 0

      // Return count of XMAS found
      right + left + top + bottom + topLeft + topRight + bottomRight + bottomLeft
    }
  }

  def countMasAt(input: Seq[String], x: Int, y: Int): Int = {
    val maxX = input.head.length - 1
    val maxY = input.size - 1
    if (input(y).charAt(x) != 'A') // Check that current position is A
      0
    else if (y + 1 > maxY || y - 1 < 0 || x + 1 > maxX || x - 1 < 0) // Check neighbourhood are available
      0
    else {// Check the word MAS in diagonal centered on A at (x, y)
      val a =
        if (input(y + 1).charAt(x - 1) == 'M' && input(y - 1).charAt(x + 1) == 'S')
          1
        else
          0
      val b =
        if (input(y + 1).charAt(x - 1) == 'S' && input(y - 1).charAt(x + 1) == 'M')
          1
        else
          0
      val c =
        if (input(y + 1).charAt(x + 1) == 'M' && input(y - 1).charAt(x - 1) == 'S')
          1
        else
          0
      val d =
        if (input(y + 1).charAt(x + 1) == 'S' && input(y - 1).charAt(x - 1) == 'M')
          1
        else
          0

      // Return 1 if we have found at least 2 MAS
      if (a + b + c + d > 1) 1 else 0
    }
  }

}
