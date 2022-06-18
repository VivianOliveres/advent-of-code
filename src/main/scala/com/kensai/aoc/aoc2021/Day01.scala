package com.kensai.aoc.aoc2021

object Day01 {

  /** Compute the multiplication of the two numbers from {@code candidates}
    * that the sum is equal to {@code expectedSum}.
    *
    * Return {@code None} if the expected sum cannot be found.
    */
  def compute(expectedSum: Long, candidates: List[Long]): Option[Long] = {
    val sortedCandidates = candidates.sorted
    var firstIndex = 0
    var lastIndex = sortedCandidates.size - 1

    while (firstIndex < lastIndex) {
      val sum = sortedCandidates(firstIndex) + sortedCandidates(lastIndex)
      if (sum == expectedSum)
        return Some(sortedCandidates(firstIndex) * sortedCandidates(lastIndex))
      else {
        if (sum > expectedSum) lastIndex = lastIndex - 1
        else firstIndex = firstIndex + 1
      }
    }
    None
  }

  /** Compute the multiplication of the three numbers from {@code candidates}
    * that the sum is equal to {@code expectedSum}.
    *
    * Return {@code None} if the expected sum cannot be found.
    */
  def compute3(expectedSum: Long, candidates: List[Long]): Option[Long] =
    candidates
      .map(number => compute(expectedSum - number, candidates).map(_ * number))
      .find(_.isDefined)
      .flatten

}
