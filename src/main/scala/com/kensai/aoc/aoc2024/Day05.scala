package com.kensai.aoc.aoc2024

import scala.annotation.tailrec

object Day05 {

  case class Day5Input(rules: Map[Int, Seq[Int]], updates: Seq[Seq[Int]])

  private val ruleRegex = """(\d+)\|(\d+)""".r

  def parse(inputStr: String): Day5Input = {
    val split    = inputStr.split("\n\n")

    // Parse rules
    val rulesStr = split.head
    val rules = rulesStr
      .split("\n")
      .toSeq
      .map(_.trim).filterNot(_.isEmpty)
      .map {
        case ruleRegex(x, y) => x.toInt -> y.toInt
        case str             => throw new IllegalArgumentException(s"Invalid input for regex: [$str]")
      }
      .groupBy(_._1).view.mapValues(v => v.map(_._2)).toMap

    // Parse updates
    val updatesStr = split(1)
    val updates = updatesStr
      .split("\n")
      .toSeq
      .map(_.trim).filterNot(_.isEmpty)
      .map(_.split(",").toSeq.map(_.toInt))

    Day5Input(rules, updates)
  }

  def sumValidMidPages(input: Day5Input): Int =
    filterRules(input = input, keepValid = true)
      .map(update => update(update.size / 2))
      .sum

  def filterRules(input: Day5Input, keepValid: Boolean): Seq[Seq[Int]] =
    doFilterValidRules(input, 0, Seq(), keepValid)

  @tailrec
  private def doFilterValidRules(input: Day5Input, updateIndex: Int, acc: Seq[Seq[Int]], keepValid: Boolean): Seq[Seq[Int]] =
    if (updateIndex >= input.updates.size)
      acc
    else {
      val isUpdateValid = validateUpdate(input.rules, input.updates(updateIndex), 0, Seq())
      if (isUpdateValid == keepValid)
        doFilterValidRules(input, updateIndex + 1, acc :+ input.updates(updateIndex), keepValid)
      else
        doFilterValidRules(input, updateIndex + 1, acc, keepValid)
    }

  @tailrec
  def validateUpdate(rules: Map[Int, Seq[Int]], update: Seq[Int], index: Int, validated: Seq[Int]): Boolean =
    if (index >= update.size)
      true
    else {
      val element    = update(index)
      val maybeRules = rules.get(element)
      if (maybeRules.isEmpty)
        validateUpdate(rules, update, index + 1, validated :+ element)
      else {
        val rulesToCheck = maybeRules.get
        val fail         = rulesToCheck.find(validated.contains)
        if (fail.nonEmpty)
          false
        else
          validateUpdate(rules, update, index + 1, validated :+ element)
      }
    }

  def sumReorderedInvalidMidPages(input: Day5Input): Int =
    filterRules(input = input, keepValid = false)
      .map(invalidUpdate => reorder(input.rules, invalidUpdate))
      .map(update => update(update.size / 2))
      .sum

  def reorder(rules: Map[Int, Seq[Int]], update: Seq[Int]): Seq[Int] =
    doReorder(rules, update, 0, Seq(), Seq())

  @tailrec
  def doReorder(rules: Map[Int, Seq[Int]], update: Seq[Int], index: Int, validated: Seq[Int], acc: Seq[Int]): Seq[Int] =
    if (index >= update.size) {
      if (validateUpdate(rules, acc, 0, Seq()))
        acc
      else // If not valid => reorder again
        doReorder(rules, acc, 0, Seq(), Seq())
    } else {
      val element    = update(index)
      val maybeRules = rules.get(element)
      if (maybeRules.isEmpty)
        doReorder(rules, update, index + 1, validated :+ element, acc :+ element)
      else {
        val rulesToCheck = maybeRules.get
        val fail         = rulesToCheck.find(validated.contains)
        if (fail.nonEmpty) {
          // Move the invalid element one step behind
          // Not sure that will fix the ordering => So check and reorder if needed
          val updatedAcc = acc.take(acc.size - 1) :+ element :+ acc.last
          doReorder(rules, update, index + 1, validated :+ element, updatedAcc)
        } else
          doReorder(rules, update, index + 1, validated :+ element, acc :+ element)
      }
    }

}
