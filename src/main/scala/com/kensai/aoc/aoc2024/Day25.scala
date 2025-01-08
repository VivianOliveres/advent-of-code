package com.kensai.aoc.aoc2024

object Day25 {

  case class Schematics(c1: Int, c2: Int, c3: Int, c4: Int, c5: Int) {
    def sum(other: Schematics): Schematics =
      Schematics(c1 + other.c1, c2 + other.c2, c3 + other.c3, c4 + other.c4, c5 + other.c5)
    def isComplementary: Boolean =
      c1 <= 5 && c2 <= 5 && c3 <= 5 && c4 <= 5 && c5 <= 5
  }

  case class Day25Input(keys: Seq[Schematics], locks: Seq[Schematics])

  def parse(input: String): Day25Input = {
    val schemas = input.split("\n\n").toSeq
    val (allKeys, allLocks) = schemas.foldLeft((Seq.empty[Schematics], Seq.empty[Schematics])){case ((keys, locks), schema) =>
      val lines = schema.split("\n").toSeq
      val isLock = lines.head.count(_ == '#') > 3
      val schematics = if (isLock) {
        val values = (0 to 4).map{column =>
          (0 to 6).find{row => lines(row)(column) == '.'}.getOrElse(-1) - 1
        }
        Schematics(values.head, values(1), values(2), values(3), values(4))
      } else {
        val values = (0 to 4).map{column =>
          5 - (6 to 0 by -1).find{row => lines(row)(column) == '.'}.getOrElse(6)
        }
        Schematics(values.head, values(1), values(2), values(3), values(4))
      }

      if (isLock)
        (keys, locks :+ schematics)
      else
        (keys :+ schematics, locks)
    }

    Day25Input(allKeys, allLocks)
  }

  def computeUnique(input: Day25Input): Int = {
    input.locks.
      flatMap{lock => input.keys.map(key => (lock, key, lock.sum(key)))}
      .count(_._3.isComplementary)
  }

}
