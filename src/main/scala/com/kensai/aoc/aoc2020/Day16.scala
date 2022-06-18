package com.kensai.aoc.aoc2020

object Day16 {

  //TODO: clean everything

  case class Board(
      rules: List[Rule],
      yourTicket: Ticket,
      nearbyTickets: List[Ticket]
  )

  case class Ticket(values: List[Long]) {
    def isValid(rules: List[Rule]): Boolean =
      values.forall(value => rules.exists(_.isValid(value)))
    def validRules(rules: List[Rule]): Map[Rule, Set[Int]] =
      rules
        .map(r =>
          r -> values.zipWithIndex.filter(v => r.isValid(v._1)).map(_._2).toSet
        )
        .toMap
  }
  object Ticket {
    def apply(line: String): Ticket =
      Ticket(line.split(",").map(_.trim).map(_.toLong).toList)
  }

  case class Rule(
      name: String,
      min1: Long,
      max1: Long,
      min2: Long,
      max2: Long
  ) {
    def isValid(value: Long): Boolean =
      (min1 <= value && value <= max1) || (min2 <= value && value <= max2)

    def isValid(ticket: Ticket): Boolean =
      ticket.values.exists(isValid)

    def invalidValue(ticket: Ticket): Long =
      ticket.values.find(!isValid(_)).getOrElse(0L)
  }

  private val ruleRegex = """([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  def doParseRule(line: String): Rule = line match {
    case ruleRegex(name, min1, max1, min2, max2) =>
      Rule(name, min1.toLong, max1.toLong, min2.toLong, max2.toLong)
    case _ => throw new RuntimeException(s"Unknown rule [$line]")
  }

  def parse(inputs: String): Board = {
    val splited = inputs.split("\n\n").toList
    val rules = splited.head
      .split("\n")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(doParseRule)
      .toList
    val yourTicketPart = splited(1)
      .split("\n")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .tail
      .map(Ticket(_))
      .head
    val nearbyTicketPart = splited(2)
      .split("\n")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .tail
      .map(Ticket(_))
      .toList
    Board(rules, yourTicketPart, nearbyTicketPart)
  }

  def doSumFirstInvalidValues(ticket: Ticket, rules: List[Rule]): Long = {
    ticket.values
      .map(value => rules.find(_.isValid(value)).map(_ => 0L).getOrElse(value))
      .sum
  }

  def sumFirstInvalidValues(input: String): Long = {
    val board = parse(input)
    board.nearbyTickets
      .map(doSumFirstInvalidValues(_, board.rules))
      .sum
  }

  def doFieldsPosition(
      ticket: Ticket,
      rules: List[Rule]
  ): Option[Map[Int, Set[Rule]]] = {
    val result = ticket.values.zipWithIndex.map {
      case (value, index) => {
        index -> rules.filter(r => r.isValid(value)).toSet
      }
    }.toMap
    if (result.exists(_._2.isEmpty) || result.size != rules.size)
      None
    else if (result.flatMap(_._2).toSet.size != rules.size)
      None
    else
      Some(replaceSolutions(result))
  }

  /** When an index contains only one rule, try to remove this rule for every other indexes that contains it.
    */
  def replaceSolutions(input: Map[Int, Set[Rule]]): Map[Int, Set[Rule]] = {
    var tmp = input
    var alreadyReduced: Set[Rule] = Set()
    var toRemove = input
      .filter(_._2.size == 1)
      .flatMap(_._2)
      .filterNot(rule => alreadyReduced.contains(rule))
      .toList
    while (toRemove.nonEmpty) {
      toRemove.foreach(rule => {
        tmp
          .filter(tuple => tuple._2.contains(rule) && tuple._2.size > 1)
          .foreach {
            case (index, rules) => {
              tmp = tmp.updated(index, rules - rule)
            }
          }
        alreadyReduced = alreadyReduced + rule
      })
      toRemove = input
        .filter(_._2.size == 1)
        .flatMap(_._2)
        .filterNot(rule => alreadyReduced.contains(rule))
        .toList
    }
    tmp
  }

  def reduce(
      i1: Map[Int, Set[Rule]],
      i2: Map[Int, Set[Rule]]
  ): Map[Int, Set[Rule]] = {
    var result: Map[Int, Set[Rule]] = i1
    (0 until i2.size).foreach(index => {
      val rs1 = result(index)
      val rs2 = i2(index)
      val newSet = rs1.intersect(rs2)
      result = replaceSolutions(result.updated(index, newSet))
    })
    replaceSolutions(result)
  }

  def doFieldsPosition(
      tickets: List[Ticket],
      rules: List[Rule]
  ): Map[Int, Set[Rule]] = {
    val first = rules.indices.map(i => i -> rules.toSet).toMap
    val tmp = tickets
      .flatMap(doFieldsPosition(_, rules))
      .foldLeft(first)(reduce)
    replaceSolutions(tmp)
  }

  def fieldsPosition(input: String): Long = {
    val board = parse(input)
    doFieldsPosition(board.nearbyTickets, board.rules)
      .map { case (index, rules) => (index, rules.head) }
      .filter(_._2.name.startsWith("departure"))
      .map { case (index, _) => board.yourTicket.values(index) }
      .foldLeft(1L)(_ * _)
  }
}
