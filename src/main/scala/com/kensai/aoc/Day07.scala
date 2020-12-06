package com.kensai.aoc

object Day07 {

  def parseColorContainedBy(input: String): Map[String, String] = {
    val values = input.split("[ \\.]")
      .toList
      .map(_.trim)
    val colors = values.zipWithIndex
      .filter(_._1.startsWith("bag"))
      .map { case (_, index) => values(index - 2) + " " + values(index - 1) }
    colors.tail.toSet.filterNot(_ == "no other").map(_ -> colors.head).toMap
  }

  def parseColorContainedBy(inputs: List[String]): Map[String, Set[String]] = {
    inputs.map(parseColorContainedBy)
      .foldLeft(Map.empty[String, Set[String]]) { case (l, r) => combine(l, r) }
  }

  private def combine(acc: Map[String, Set[String]], other: Map[String, String]): Map[String, Set[String]] = {
    val keys = acc.keySet ++ other.keySet
    keys.map(key => key -> (acc.getOrElse(key, Set.empty[String]) ++ combineRight(key, other)))
      .toMap
  }

  private def combineRight(key: String, other: Map[String, String]): Set[String] =
    other.get(key).map(Set(_)).getOrElse(Set.empty[String])

  def bagsContaining(color: String, inputs: List[String]): Long = {
    val rules = parseColorContainedBy(inputs)
    doBagsContaining(color, rules).size
  }

  def doBagsContaining(color: String, rules: Map[String, Set[String]]): Set[String] = {
    val containsDirectly = rules.getOrElse(color, Set.empty[String])
    containsDirectly
      .map(doBagsContaining(_, rules))
      .foldLeft(containsDirectly)(_ ++ _)
  }

  def parseColorContaining(inputs: List[String]): Map[String, Map[String, Int]] =
    inputs
      .map(_.trim)
      .map(doParseColorContaining)
      .toMap

  private def doParseColorContaining(input: String): (String, Map[String, Int]) = {
    val firstSplit = input.split(" ").toList
    val head = firstSplit.head + " " + firstSplit(1)

    val secondSplit = input.split("contain")
      .toList
      .tail // Second part of the "contain"
      .head
      .replaceAll("bags", "")
      .replaceAll("bag", "")
      .replaceAll("\\.", "")
      .split(",")
      .toList
      .map(_.trim)
      .filterNot(_ == "no other")
      .map(s => {
        val s2 = s.split(" ").toList
        s2(1) + " " + s2(2) -> s2.head.toInt
      }).toMap

    (head, secondSplit)
  }

  private def doComputeBagContainingOtherBAgs(color: String, rules: Map[String, Map[String, Int]]): Long =
    rules
      .get(color)
      .map(sumBags(_, rules))
      .orElse(Some(0L))
      .map(_ + 1L) // Add itself
      .get

  private def sumBags(elements: Map[String, Int], rules: Map[String, Map[String, Int]]): Long =
    elements.toList.map { case (key, count) => count * doComputeBagContainingOtherBAgs(key, rules) }.sum

  def computeBagContainingOtherBags(color: String, inputs: List[String]): Long = {
    val rules = parseColorContaining(inputs)
    doComputeBagContainingOtherBAgs(color, rules) - 1 // Remove itself
  }
}
