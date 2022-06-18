package com.kensai.aoc.aoc2020

object Day06 {

  /** Parse the input to return the list of different answers per group.
    */
  private def parseAnswersPerGroup(input: String): List[Set[Char]] =
    parseAnswersPerPerson(input)
      .map(_.foldLeft(Set[Char]())(_.union(_)))

  /** Sum the different answers for each group.
    */
  def sumDifAnswers(input: String): Long =
    parseAnswersPerGroup(input).map(_.size).sum.toLong

  /** Parse the input to return the list of different answers per person.
    */
  private def parseAnswersPerPerson(input: String): List[List[Set[Char]]] =
    input
      .split("\n\n")
      .toList
      .map(_.trim)
      .map(_.split("\n").toList.map(_.toSet))

  /** Compute the number of answers given by every body in a group, and sum them over the group.
    */
  def sumEveryoneAnswers(input: String): Long =
    parseAnswersPerPerson(input)
      .map(s => countInGroup(s))
      .sum

  private def countInGroup(personAnswers: List[Set[Char]]): Long =
    personAnswers.foldLeft(personAnswers.head)(_.intersect(_)).size.toLong
}
