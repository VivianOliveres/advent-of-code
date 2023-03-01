package com.kensai.aoc.aoc2021

object Day09 {

  case class Pos(x: Int, y: Int) {
    def closestPoints: Seq[Pos] =
      Seq(Pos(x - 1, y), Pos(x + 1, y), Pos(x, y - 1), Pos(x, y + 1))

    def isReachable(matrix: Seq[Seq[Int]]): Boolean =
      x >= 0 && x < matrix.size && y >= 0 && y < matrix.head.size

    def value(matrix: Seq[Seq[Int]]): Int =
      matrix(x)(y)
  }

  private def parse(inputs: Seq[String]): Seq[Seq[Int]] =
    inputs.filterNot(_.isEmpty).map { line =>
      line.toArray.map(_.toInt).map(_ - 48).toSeq
    }

  /** Return the value of the lowest points.
    */
  def findLowestPoints(inputs: Seq[String]): Seq[Int] =
    findLowestAndBasins(inputs).map(_._1)

  private def isLowest(value: Int, pos: Pos, matrix: Seq[Seq[Int]]): Boolean =
    pos.closestPoints
      .filter(_.isReachable(matrix))
      .map { case Pos(x, y) => matrix(x)(y) }
      .forall(matrixValue => value < matrixValue)

  /** Sum the value of each lowest points.
    */
  def computeRiskLevel(inputs: Seq[String]): Int =
    findLowestPoints(inputs).map(_ + 1).sum

  /** Find the lowest points and sum the closest points tahat do not have a 9 value.
    */
  def findLowestAndBasins(inputs: Seq[String]): Seq[(Int, Int)] = {
    def matrix = parse(inputs)
    for {
      (line, x)  <- matrix.zipWithIndex
      (value, y) <- line.zipWithIndex
      pos = Pos(x, y)
      if isLowest(value, pos, matrix)
      basins = findBasinsSize(pos, matrix)
    } yield (value, basins)
  }

  private def findBasinsSize(pos: Pos, matrix: Seq[Seq[Int]]): Int = {
    var count             = 0
    var visited: Set[Pos] = Set()
    var toVisit: Set[Pos] = Set(pos)
    while (toVisit.nonEmpty) {
      val head = toVisit.head
      count = count + 1
      visited = visited + head
      val nextPositions = head.closestPoints
        .filter(_.isReachable(matrix))
        .filterNot(otherPos => visited.contains(otherPos) || otherPos.value(matrix) == 9)
      toVisit = toVisit.tail ++ nextPositions
    }
    count
  }

  /** Find the 3 largest basins and multiply them.
    */
  def multiply3LargestBasinsSize(inputs: Seq[String]): Int =
    findLowestAndBasins(inputs).map(_._2).sortBy(-_).take(3).product
}
