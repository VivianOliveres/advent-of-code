package com.kensai.aoc

object Day02 {

  case class PasswordRow(lowestValue: Int, higherValue: Int, value: Char, password: String)

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
   * Check that input is a valid {@code PasswordRow}.
   */
  def isPasswordValid(candidate: PasswordRow): Boolean = {
    val count = candidate.password.count(p => p == candidate.value)
    candidate.lowestValue <= count && count <= candidate.higherValue
  }

  /**
   * Count the number of valid {@code PasswordRow} from {@code candidates}.
   */
  def validPasswordCount(candidates: List[PasswordRow]): Int =
    candidates.count(isPasswordValid)
}
