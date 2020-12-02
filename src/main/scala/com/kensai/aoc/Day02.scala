package com.kensai.aoc

object Day02 {

  case class PasswordRow(firstIndex: Int, lastIndex: Int, expectedChar: Char, password: String)

  /**
   * Parse the {@code input} string into a {@code PasswordRow}.<br>
   * Return {@code None} if the string has an invalid format.
   */
  def parse(input: String): Option[PasswordRow] = {
    try {
      val split = input.split("[- :]").toList.filterNot(_.isEmpty)
      val lowestValue = split(0).toInt
      val higherValue = split(1).toInt
      val value = split(2).head
      val password = split(3)
      Some(PasswordRow(lowestValue, higherValue, value, password))

    } catch {
      case _: RuntimeException => {
        System.err.println(s"Day02 - parse - Invalid format for input[$input]")
        None
      }
    }
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
