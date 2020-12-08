package com.kensai.aoc

object Day02 {

  case class PasswordRow(firstIndex: Int, lastIndex: Int, expectedChar: Char, password: String)

  private val rowRegex = """(\d+)-(\d+) ([a-z]): ([a-z]+)""".r

  /**
   * Parse the {@code input} string into a {@code PasswordRow}.<br>
   * Return {@code None} if the string has an invalid format.
   */
  def parse(input: String): Option[PasswordRow] =
    input match {
      case rowRegex(lowestValue, higherValue, value, password) => Some(PasswordRow(lowestValue.toInt, higherValue.toInt, value.head, password))
      case _ => None
    }

  /**
   * Check that input is a valid {@code PasswordRow}.<br>
   * It checks that {@code PasswordRow.expectedChar} is present between
   * {@code PasswordRow.firstIndex} and {@code PasswordRow.lastIndex} times into
   * {@code PasswordRow.password}.
   */
  def isPasswordValidPart1(candidate: PasswordRow): Boolean = {
    val count = candidate.password.count(p => p == candidate.expectedChar)
    candidate.firstIndex <= count && count <= candidate.lastIndex
  }

  /**
   * Check that input is a valid {@code PasswordRow}.<br>
   * It checks that {@code PasswordRow.expectedChar} is present at
   * {@code PasswordRow.firstIndex} or {@code PasswordRow.lastIndex} into
   * {@code PasswordRow.password} but not at both positions.
   */
  def isPasswordValidPart2(candidate: PasswordRow): Boolean =
    candidate.password(candidate.firstIndex - 1) == candidate.expectedChar ^ candidate.password(candidate.lastIndex - 1) == candidate.expectedChar

  /**
   * Count the number of valid {@code PasswordRow} from {@code candidates}.
   */
  def validPasswordCount(candidates: List[PasswordRow])(validator: PasswordRow => Boolean): Int =
    candidates.count(validator)
}
