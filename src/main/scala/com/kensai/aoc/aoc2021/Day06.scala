package com.kensai.aoc.aoc2021

object Day06 {

  def computeFishesCount(line: String, nbDays: Int): Long = {
    val timers = line
      .split("\n")
      .filterNot(_.isEmpty)
      .map(_.trim)
      .head
      .split(",")
      .map(_.toInt)
      .toSeq
    doCompute(timers, nbDays)
  }

  private def doCompute(timers: Seq[Int], nbDays: Int): Long = {
    // Map[Day, Count of fishs]
    val tmp: collection.mutable.Map[Int, Long] = collection.mutable.Map()
    timers.foreach { timer =>
      val dayOfProcreation = nbDays - timer
      tmp.put(dayOfProcreation, tmp.getOrElse(dayOfProcreation, 0L) + 1L)
    }

    // For each day decremental
    (nbDays to 1 by -1).foreach { day =>
      // Get thenumber of fishes for today
      val count = tmp.getOrElse(day, 0L)

      // Update them in 6/7 days
      val count6 = tmp.getOrElse(day - 7, 0L)
      tmp.put(day - 7, count6 + count)

      // Schedule the new fishes in 8/9 days
      val count8 = tmp.getOrElse(day - 9, 0L)
      tmp.put(day - 9, count8 + count)

      // Clean old state for final sum
      tmp.put(day, 0L)
    }

    tmp.values.sum
  }

}
