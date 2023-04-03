package com.kensai.aoc.aoc2022

object Day01 {

  case class BufferDay01(elvesCalories: Seq[Int] = Seq(), acc: Int = 0) {

    def addNewElf: BufferDay01 =
      BufferDay01(elvesCalories :+ acc, 0)
  }

  def sumMaxCalories(rows: List[String], count: Int): Int = {
    val result = rows
      .map(_.trim)
      .foldLeft(BufferDay01()) { case (buffer, line) =>
        if (line == "")
          buffer.addNewElf
        else
          buffer.copy(acc = buffer.acc + line.toInt)
      }
      .addNewElf // empty the accumulator into a new elf

    result.elvesCalories
      .sortBy(-_)
      .take(count)
      .sum
  }

}
