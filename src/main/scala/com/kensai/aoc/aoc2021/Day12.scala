package com.kensai.aoc.aoc2021

object Day12 {

  case class Cave(name: String) {
    def isSmall: Boolean =
      name.head.isLower

    def isStart: Boolean =
      name == "start"

    def isEnd: Boolean =
      name == "end"
  }

  case class Path(from: Cave, to: Cave)

  private val pathRegex = """([a-zA-Z]*)-([a-zA-Z]*)""".r
  private def parseInputs(lines: Seq[String]): Seq[Path] =
    lines.filterNot(_.isEmpty).flatMap {
      case pathRegex(left, right) =>
        Seq(Path(Cave(left), Cave(right)), Path(Cave(right), Cave(left)))
      case _ => throw new IllegalArgumentException(s"Invalid line: [$lines]")
    }

  /** Count number of different path from "start" to "end" where every small cave can only be accessed once.
    */
  def countUniqPaths(lines: Seq[String]): Int = {
    val paths           = parseInputs(lines)
    val pathByFirstCave = paths.groupBy(_.from)
    val solutions =
      doComputePathsToEnd(Cave("start"), pathByFirstCave, Seq(), Set())
    solutions.size
  }

  /** @param currentCave
    *   : The cave at this step
    * @param paths
    *   : all the available paths grouped by cave
    * @param currentPath
    *   : sequence of caves visited
    * @param smallCaveVisited
    *   : set of small cave visited
    * @return
    *   all the sequences of caves from "start" to "end"
    */
  private def doComputePathsToEnd(
      currentCave: Cave,
      paths: Map[Cave, Seq[Path]],
      currentPath: Seq[Cave],
      smallCaveVisited: Set[Cave]
    ): Seq[Seq[Cave]] =
    if (currentCave.isEnd)
      Seq(currentPath :+ currentCave)
    else {
      val possiblePaths = paths(currentCave)
      possiblePaths.foldLeft(Seq.empty[Seq[Cave]]) { case (acc, path) =>
        if (smallCaveVisited.contains(path.to))
          acc
        else
          acc ++ doComputePathsToEnd(
            path.to,
            paths,
            currentPath :+ path.to,
            if (currentCave.isSmall) smallCaveVisited + currentCave
            else smallCaveVisited
          )
      }
    }

  /** Start cave can only be access once. Each small cave can only be accessed once except one.
    * @return
    *   the number of different path from "start" to "end"
    */
  def countUniqPaths2(lines: Seq[String]): Int = {
    val paths           = parseInputs(lines)
    val pathByFirstCave = paths.groupBy(_.from)
    doComputePathsToEnd2(Cave("start"), pathByFirstCave, Seq(), Set(), None).size
  }

  /** @param currentCave
    *   : The cave at this step
    * @param paths
    *   : all the available paths grouped by cave
    * @param currentPath
    *   : sequence of caves visited
    * @param smallCaveVisited
    *   : set of small cave visited
    * @param caveVisitedTwice
    *   : the only one small cave visited twice
    * @return
    *   all the sequences of caves from "start" to "end"
    */
  private def doComputePathsToEnd2(
      currentCave: Cave,
      paths: Map[Cave, Seq[Path]],
      currentPath: Seq[Cave],
      smallCaveVisited: Set[Cave],
      caveVisitedTwice: Option[Cave]
    ): Seq[Seq[Cave]] =
    if (currentCave.isEnd)
      Seq(currentPath :+ currentCave)
    else {
      val possiblePaths = paths(currentCave)
      possiblePaths.foldLeft(Seq.empty[Seq[Cave]]) { case (acc, path) =>
        if (currentPath.nonEmpty && currentCave.isStart)
          acc
        else if (smallCaveVisited.contains(path.to) && caveVisitedTwice.isDefined)
          acc
        else if (smallCaveVisited.contains(path.to))
          acc ++ doComputePathsToEnd2(
            path.to,
            paths,
            currentPath :+ path.to,
            if (currentCave.isSmall) smallCaveVisited + currentCave
            else smallCaveVisited,
            Some(currentCave)
          )
        else
          acc ++ doComputePathsToEnd2(
            path.to,
            paths,
            currentPath :+ path.to,
            if (currentCave.isSmall) smallCaveVisited + currentCave
            else smallCaveVisited,
            caveVisitedTwice
          )
      }
    }

}
