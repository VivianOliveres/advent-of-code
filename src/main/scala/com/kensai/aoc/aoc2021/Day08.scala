package com.kensai.aoc.aoc2021

object Day08 {

  case class DigitValues(clues: Set[Digits], value: Seq[Digits])
  case class Digits(values: Set[Char]) {
    val isUnique: Boolean =
      values.size == 2 || values.size == 3 || values.size == 4 || values.size == 7

    val size = values.size

    def diff(other: Digits, bits: Char*): Set[Char] =
      values.diff(other.values).diff(bits.toSet)

    def diff(other1: Digits, other2: Digits, bits: Char*): Set[Char] =
      values.diff(other1.values ++ other2.values).diff(bits.toSet)

    def diff(bits: Char*): Set[Char] =
      values.diff(bits.toSet)
  }

  def parseInput(inputs: Seq[String]): Seq[DigitValues] =
    inputs.collect {
      case str if str.nonEmpty => str.trim
    }.map { line =>
      val splitted    = line.split(" \\| ")
      val leftDigits  = splitted.head.split(" ").map(i => Digits(i.toSet)).toSet
      val rightDigits = splitted(1).split(" ").map(i => Digits(i.toSet)).toSeq
      DigitValues(leftDigits, rightDigits)
    }

  def countDigits147(inputs: Seq[String]): Int = {
    val values = parseInput(inputs)
    values
      .flatMap(_.value)
      .count(_.isUnique)
  }

  def computeDigitValue(values: DigitValues): Int = {
    val digit1 = values.clues
      .filter(_.values.size == 2)
      .head
    val digit4 = values.clues
      .filter(_.values.size == 4)
      .head
    val digit7 = values.clues
      .filter(_.values.size == 3)
      .head
    val digit8 = values.clues
      .filter(_.values.size == 7)
      .head

    val a = digit7.diff(digit1).head

    val digit235s = values.clues
      .groupBy(_.values.size)
      .map(v => (v._1, v._2))
      .filter(_._1 == 5) // Keep 2, 3, 5 digits
      .flatMap(_._2)
      .toSet
    assert(digit235s.size == 3)
    val digit069s = values.clues
      .groupBy(_.values.size)
      .map(v => (v._1, v._2))
      .filter(_._1 == 6) // Keep 0, 6, 9 digits
      .flatMap(_._2)
      .toSet
    assert(digit069s.size == 3)

    val g = digit235s
      .map(_.diff(digit7, digit4))
      .filter(_.size == 1)
      .head
      .head

    val e = digit235s
      .map(_.diff(digit7, digit4, g))
      .filter(_.size == 1)
      .head
      .head

    val b = digit069s
      .map(_.diff(digit7, g, e))
      .filter(_.size == 1)
      .head
      .head

    val d = digit4.diff(digit1, b).head

    val seq069 = digit069s.toSeq
    val seq069digitsDiff =
      seq069(0).diff(seq069(1)) ++ seq069(1).diff(seq069(0)) ++ seq069(1).diff(
        seq069(2)
      ) ++ seq069(2).diff(seq069(1))
    val c = Digits(seq069digitsDiff).diff(d, e).head

    val f = digit1.diff(c).head

    val digit0 = Digits(Set(a, b, c, e, f, g))
    val digit2 = Digits(Set(a, c, d, e, g))
    val digit3 = Digits(Set(a, c, d, f, g))
    val digit5 = Digits(Set(a, b, d, f, g))
    val digit6 = Digits(Set(a, b, d, e, f, g))
    val digit9 = Digits(Set(a, b, c, d, f, g))

    val resultSeq = values.value.foldLeft(Seq(0)) { case (acc, digit) =>
      digit match {
        case `digit0` => acc :+ 0
        case `digit1` => acc :+ 1
        case `digit2` => acc :+ 2
        case `digit3` => acc :+ 3
        case `digit4` => acc :+ 4
        case `digit5` => acc :+ 5
        case `digit6` => acc :+ 6
        case `digit7` => acc :+ 7
        case `digit8` => acc :+ 8
        case `digit9` => acc :+ 9
        case _        => throw new RuntimeException(s"Should not happen!!! [$digit]")
      }
    }

    resultSeq.tail.mkString.toInt
  }

  def computeDigitValue(inputs: Seq[String]): Int = {
    val values = parseInput(inputs)
    values.map(computeDigitValue).sum
  }
}
