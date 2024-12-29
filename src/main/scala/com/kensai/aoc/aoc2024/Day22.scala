package com.kensai.aoc.aoc2024

object Day22 {

  def computeNextSecretNumber(secretNumber: Long): Long = {
    val step1      = secretNumber * 64L
    val step1Mix   = step1 ^ secretNumber
    val step1Prune = step1Mix % 16777216L

    val step2      = step1Prune / 32L
    val step2Mix   = step2 ^ step1Prune
    val step2Prune = step2Mix % 16777216L

    val step3      = step2Prune * 2048
    val step3Mix   = step3 ^ step2Prune
    val step3Prune = step3Mix % 16777216L

    step3Prune
  }

  def computeSecretNumber(secretNumber: Long, times: Int): Long =
    (1 to times).foldLeft(secretNumber) { case (previousSecretNumber, _) =>
      computeNextSecretNumber(previousSecretNumber)
    }

  def sumBuyersSecretNumbers(secretNumbers: Seq[Long], times: Int): Long =
    secretNumbers
      .map(secretNumber => computeSecretNumber(secretNumber, times))
      .sum

  // Use Vector instead of Seq for perf reasons (ie: 27s to 4s)
  def sumBananasSequences(secretNumbers: Seq[Long], times: Int): Int = {
    val allSecretNumbers: Seq[Vector[Long]] = secretNumbers.map { initialSecretNumber =>
      (1 to times).foldLeft(Vector(initialSecretNumber))((acc, _) => acc :+ computeNextSecretNumber(acc.last))
    }

    // Extract last digit
    val prices: Seq[Vector[Int]] = allSecretNumbers.map(_.map(_ % 10).map(_.toInt))

    val diffs: Seq[Vector[Int]] = prices.map(priceRow =>
      priceRow.zip(priceRow.tail).map { case (x0, x1) =>
        x1 - x0
      }
    )

    val sequenceMaps: Seq[Map[Vector[Int], Int]] = diffs.map { deltaRow =>
      val sliding = deltaRow.sliding(4)

      sliding.zipWithIndex.foldLeft(Map.empty[Vector[Int], Int]) { case (map, (sequence, index)) =>
        if (map.contains(sequence))
          map // Do not update as we are looking for first Seq
        else
          map + (sequence -> index)
      }
    }

    val pricesWithSequenceIndexes: Seq[(Vector[Int], Map[Vector[Int], Int])] =
      prices.zip(sequenceMaps)

    val allSequences = diffs.flatMap(_.sliding(4))

    val sequenceCounts = allSequences.groupBy(identity).map { case (k, v) => (k, v.size) }

    val sortedSequenceCounts = sequenceCounts.toVector.sortBy(_._2).reverse

    val result = sortedSequenceCounts
      .map(_._1)
      .take(sortedSequenceCounts.size / 100)
      .map { sequence =>
        pricesWithSequenceIndexes.map { case (priceRow, sequenceIndex) =>
          sequenceIndex.get(sequence) match {
            case Some(i) => priceRow(i + 4)
            case None    => 0
          }
        }.sum
      }
      .max

    result
  }
}
