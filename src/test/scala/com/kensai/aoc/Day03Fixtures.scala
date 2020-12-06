package com.kensai.aoc

import com.kensai.aoc.Day03.TobogganRow

trait Day03Fixtures {

  val Row1: String = "..##......."
  val ExpectedRow1: TobogganRow = TobogganRow(0, 11, Set(2, 3))

  val Row2: String = "#...#...#.."
  val ExpectedRow2: TobogganRow = TobogganRow(1, 11, Set(0, 4, 8))

}
