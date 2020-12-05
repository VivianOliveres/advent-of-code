package com.kensai.aoc

object Day05 {

  /**
   * Compute the row index from the given boarding pass.
   */
  def computeRow(boardingPass: String): Long = doCompute(boardingPass.substring(0, boardingPass.length - 3), 0, 127)

  def doCompute(boardingPass: String, firstIndex: Long, lastIndex: Long): Long = {
    if (boardingPass.isEmpty)
      firstIndex
    else {
      boardingPass.head match {
        case 'F' => doComputeUpper(boardingPass, firstIndex, lastIndex)
        case 'B' => doComputeLower(boardingPass, firstIndex, lastIndex)
        case 'L' => doComputeUpper(boardingPass, firstIndex, lastIndex)
        case 'R' => doComputeLower(boardingPass, firstIndex, lastIndex)
      }
    }
  }

  private def doComputeUpper(boardingPass: String, firstIndex: Long, lastIndex: Long): Long =
    doCompute(boardingPass.tail, firstIndex, ((firstIndex + lastIndex) / 2.0).round)

  private def doComputeLower(boardingPass: String, firstIndex: Long, lastIndex: Long): Long =
    doCompute(boardingPass.tail, (firstIndex + (lastIndex - firstIndex) / 2.0).round, lastIndex)

  /**
   * Compute the column index from the given boarding pass.
   */
  def computeColumn(input: String): Long =
    doCompute(input.substring(input.length - 3), 0, 7)

  /**
   * Compute the seatId from the given boarding pass.
   */
  def computeSeatId(input: String): Long =
    doComputeSeatId(computeRow(input), computeColumn(input))

  def doComputeSeatId(row: Long, column: Long): Long =
    row * 8 + column

  /**
   * Return the highest seatId from the given boarding passes.
   */
  def computeHighestSeatId(inputs: List[String]): Long =
    inputs.map(computeSeatId).max

  /**
   * Return the missing SeatId from all the given boarding passes.
   */
  def findSeatId(inputs: List[String]): Long = {
    val cellsByRowIndex = inputs.map(i => (computeRow(i), computeColumn(i), computeSeatId(i)))
      .groupBy(_._1)
      .filter(_._2.size != 8)
      .map { case (row, other) => (row, other.groupBy(_._2)) }
      .head

    val indexColumns = cellsByRowIndex._2.keySet
    val missingColumn = (0L to 7L).toSet.diff(indexColumns).toList.head

    val row = cellsByRowIndex._1
    doComputeSeatId(row, missingColumn)
  }
}
