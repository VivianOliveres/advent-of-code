package com.kensai.aoc.aoc2021

object Day07 {

  //shiny gold bags contain XYZ
  private val headColorRegex = """([a-z ]+) bags contain (.*)\.""".r

  //3 faded blue bags
  //3 faded blue bag
  private val countColorsRegex = """(\d+) ([a-z ]+) bags?""".r

  def parseColorContainedBy(input: String): Map[String, String] = {
    val (headColor, colorCount) = doParseColorContaining(input)
    colorCount.keySet
      .map(containedColor => containedColor -> headColor)
      .toMap
  }

  def parseColorContainedBy(inputs: List[String]): Map[String, Set[String]] =
    inputs
      .map(parseColorContainedBy)
      .foldLeft(Map.empty[String, Set[String]]) { case (l, r) => combine(l, r) }

  private def combine(
      acc: Map[String, Set[String]],
      other: Map[String, String]
  ): Map[String, Set[String]] = {
    val keys = acc.keySet ++ other.keySet
    keys
      .map(key =>
        key -> (acc
          .getOrElse(key, Set.empty[String]) ++ combineRight(key, other))
      )
      .toMap
  }

  private def combineRight(
      key: String,
      other: Map[String, String]
  ): Set[String] =
    other.get(key).map(Set(_)).getOrElse(Set.empty[String])

  def bagsContaining(color: String, inputs: List[String]): Long = {
    val rules = parseColorContainedBy(inputs)
    doBagsContaining(color, rules).size.toLong
  }

  def doBagsContaining(
      color: String,
      rules: Map[String, Set[String]]
  ): Set[String] = {
    val containsDirectly = rules.getOrElse(color, Set.empty[String])
    containsDirectly
      .map(doBagsContaining(_, rules))
      .foldLeft(containsDirectly)(_ ++ _)
  }

  def parseColorContaining(
      inputs: List[String]
  ): Map[String, Map[String, Int]] =
    inputs
      .map(_.trim)
      .map(doParseColorContaining)
      .toMap

  private def parseColors(input: String): Map[String, Int] =
    input
      .split(", ")
      .filterNot(_ == "no other bags")
      .map {
        case countColorsRegex(count, color) => color -> count.toInt
        case _                              => throw new RuntimeException(s"Invalid color to parse [$input]")
      }
      .toMap

  private def doParseColorContaining(
      input: String
  ): (String, Map[String, Int]) =
    input match {
      case headColorRegex(headColor, other) => (headColor, parseColors(other))
      case _                                => throw new RuntimeException(s"Invalid color [$input]")
    }

  private def doComputeBagContainingOtherBAgs(
      color: String,
      rules: Map[String, Map[String, Int]]
  ): Long =
    rules
      .get(color)
      .map(sumBags(_, rules))
      .orElse(Some(0L))
      .map(_ + 1L) // Add itself
      .get

  private def sumBags(
      elements: Map[String, Int],
      rules: Map[String, Map[String, Int]]
  ): Long =
    elements.toList.map { case (key, count) =>
      count * doComputeBagContainingOtherBAgs(key, rules)
    }.sum

  def computeBagContainingOtherBags(
      color: String,
      inputs: List[String]
  ): Long = {
    val rules = parseColorContaining(inputs)
    doComputeBagContainingOtherBAgs(color, rules) - 1 // Remove itself
  }
}
