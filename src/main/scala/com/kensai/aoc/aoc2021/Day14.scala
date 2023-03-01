package com.kensai.aoc.aoc2021

object Day14 {

  case class Inputs(
      initialTemplate: String,
      polymer: Map[(Char, Char), Long],
      rules: Map[(Char, Char), Rule])
  case class Rule(left: Char, right: Char, toInsert: Char)

  private val ruleRegex = """([A-Z])([A-Z]) -> ([A-Z])""".r
  def parse(lines: Seq[String]): Inputs = {
    val polymer = parsePolymer(lines.head.toCharArray.toSeq)
    val rules   = parseRules(lines.tail)
    Inputs(lines.head, polymer, rules)
  }

  /** Parse polymer. Group them by rule (ie (Char1,Char2)) and count them (ie count is 1).
    */
  private def parsePolymer(polymer: Seq[Char]): Map[(Char, Char), Long] = {
    val (results, _) =
      polymer.tail.foldLeft((Seq.empty[(Char, Char)], polymer.head)) { case (acc, currentChar) =>
        (acc._1 :+ (acc._2, currentChar), currentChar)
      }
    results.groupBy(identity).map { case (k, v) => (k, v.size.toLong) }
  }

  /** Parse rules and group them by Char.
    */
  private def parseRules(lines: Seq[String]): Map[(Char, Char), Rule] = {
    val rules = lines.filterNot(_.isEmpty).map {
      case ruleRegex(l, r, i) => Rule(l.head, r.head, i.head)
      case line =>
        throw new IllegalArgumentException(s"Invalid rule in ${line}")
    }

    rules.groupBy(r => (r.left, r.right)).map { case (key, value) =>
      (key, value.head)
    }
  }

  def computeSolution(inputs: Inputs, step: Int): Long = {
    val resultsAfterInsertion = insert(inputs, step).toSeq
      .map { case (key, count) => (key._1, count) } // deduplicate

    // After deduplication, the last char of the initial template is missing
    val fixedResults =
      resultsAfterInsertion :+ (inputs.initialTemplate.last, 1L)

    // Count distinct
    val results = fixedResults
      .groupBy(_._1)
      .map { case (key, value) => (key, value.map(_._2).foldLeft(0L)(_ + _)) }

    val max = results.maxBy(_._2)._2
    val min = results.minBy(_._2)._2
    max - min
  }

  /** Apply rules on the inputs for x steps. Then return the count of each pair of Chars (For instance: "ABC" will become Map((A,B)->1,
    * (B,C)->1)
    */
  def insert(inputs: Inputs, step: Int): Map[(Char, Char), Long] =
    (0 until step - 1).foldLeft(doInsert(inputs.polymer, inputs.rules)) { case (acc, _) =>
      doInsert(acc, inputs.rules)
    }

  private def doInsert(
      polymer: Map[(Char, Char), Long],
      rules: Map[(Char, Char), Rule]
    ): Map[(Char, Char), Long] =
    polymer.toSeq
      .flatMap { case (key, value) =>
        val r = rules(key)
        // Insert based on the rule
        Seq(((r.left, r.toInsert), value), ((r.toInsert, r.right), value))
      }
      .groupBy(v => v._1)
      .map { case (key, value) =>
        (key, value.foldLeft(0L)(_ + _._2))
      }

}
