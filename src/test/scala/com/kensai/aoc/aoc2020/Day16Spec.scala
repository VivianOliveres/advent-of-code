package com.kensai.aoc.aoc2020

import Day16._
import com.kensai.aoc.lib.Lib.readInputFile
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day16Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputFile(
    "src/test/resources/2020/Day16.input"
  )
  private lazy val specInputs = readInputFile(
    "src/test/resources/2020/Day16Spec.input"
  )

  "parse rule example" should "return valid result" in {
    val input = "class: 1-3 or 5-7"
    Given(s"Input is $input")

    When("parse")
    val result = doParseRule(input)

    Then(s"Result is ")
    result shouldBe Rule("class", 1, 3, 5, 7)
  }

  "parse for spec" should "return solution" in {
    Given(s"Input is spec")

    When("parse")
    val result = parse(specInputs)

    Then(s"Result is ")
    result shouldBe Board(
      List(
        Rule("class", 1, 3, 5, 7),
        Rule("row", 6, 11, 33, 44),
        Rule("seat", 13, 40, 45, 50)
      ),
      Ticket(List(7L, 1L, 14L)),
      List(
        Ticket(List(7L, 3L, 47L)),
        Ticket(List(40L, 4L, 50L)),
        Ticket(List(55L, 2L, 20L)),
        Ticket(List(38L, 6L, 12L))
      )
    )
  }

  "doSumFirstInvalidValues for valid ticket" should "return 0" in {
    Given(s"Input is spec")
    val rules = List(
      Rule("class", 1, 3, 5, 7),
      Rule("row", 6, 11, 33, 44),
      Rule("seat", 13, 40, 45, 50)
    )
    val ticket = Ticket(List(7L, 1L, 14L))

    When("sumFirstInvalidValues")
    val result = doSumFirstInvalidValues(ticket, rules)

    Then(s"Result is ")
    result shouldBe 0L
  }

  "invalidValue for valid ticket" should "return 0" in {
    Given(s"Input is spec")
    val ticket = Ticket(List(7L, 1L, 14L))
    val rule1 = Rule("row", 1, 3, 5, 7)
    val rule2 = Rule("class", 6, 11, 33, 44)
    val rule3 = Rule("seat", 13, 40, 45, 50)

    When("sumFirstInvalidValues")
    Then(s"Result is ")
    rule1.invalidValue(ticket) shouldBe 14L
    rule2.invalidValue(ticket) shouldBe 1L
    rule3.invalidValue(ticket) shouldBe 7L
  }

  "sumFirstInvalidValues for spec" should "return 71L" in {
    Given(s"Input is spec")

    When("sumFirstInvalidValues")
    val result = sumFirstInvalidValues(specInputs)

    Then(s"Result is ")
    result shouldBe 71L
  }

  "sumFirstInvalidValues for puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When("sumFirstInvalidValues")
    val result = sumFirstInvalidValues(puzzleInputs)

    Then(s"Result is 22977")
    result shouldBe 22977L
  }

  "doFieldsPosition for invalid ticket" should "return None" in {
    Given(s"Input is spec")
    val rule1 = Rule("class", 1, 3, 5, 7)
    val rule2 = Rule("row", 6, 11, 33, 44)
    val rule3 = Rule("seat", 13, 40, 45, 50)
    val rules = List(rule1, rule2, rule3)
    val ticket = Ticket(List(40L, 4L, 50L))

    When("doFieldsPosition")
    val result = doFieldsPosition(ticket, rules)

    Then(s"Result is None")
    result shouldBe None
  }

  "doFieldsPosition for valid ticket" should "return Some(result)" in {
    Given(s"Input is spec")
    val ruleRow = Rule("row", 1, 3, 5, 7)
    val ruleClass = Rule("class", 6, 11, 33, 44)
    val ruleSeat = Rule("seat", 13, 40, 45, 50)
    val rules = List(ruleRow, ruleClass, ruleSeat)
    val ticket = Ticket(List(7L, 3L, 47L))

    When("doFieldsPosition")
    val result = doFieldsPosition(ticket, rules)

    Then(s"Result is None")
    result shouldBe Some(
      Map(0 -> Set(ruleClass), 1 -> Set(ruleRow), 2 -> Set(ruleSeat))
    )
  }

  "doFieldsPosition XYZXYZ" should "return Some(result)" in {
    Given(s"Input is spec")
    val rule0 = Rule("row", 0, 5, 8, 19)
    val rule1 = Rule("class", 0, 1, 4, 19)
    val rule2 = Rule("seat", 0, 13, 16, 19)
    val rules = List(rule0, rule1, rule2)

    val ticket1 = Ticket(List(3L, 9L, 18L))

    When("doFieldsPosition")
    val result = doFieldsPosition(ticket1, rules)

    Then(s"Result is ")
    result.isDefined shouldBe true
    result shouldBe Some(
      Map(
        0 -> Set(rule0, rule2),
        1 -> Set(rule0, rule1, rule2),
        2 -> Set(rule0, rule1, rule2)
      )
    )
  }

  "doFieldsPosition XYZ" should "return Some(result)" in {
    Given(s"Input is spec")
    val rule0 = Rule("row", 0, 5, 8, 19)
    val rule1 = Rule("class", 0, 1, 4, 19)
    val rule2 = Rule("seat", 0, 13, 16, 19)
    val rules = List(rule0, rule1, rule2)

    val ticket1 = Ticket(List(3L, 9L, 18L))
    val tickets = List(ticket1)

    When("doFieldsPosition")
    val result = doFieldsPosition(tickets, rules)

    Then(s"Result is ")
    result shouldBe Map(
      0 -> Set(rule0, rule2),
      1 -> Set(rule0, rule1, rule2),
      2 -> Set(rule0, rule1, rule2)
    )
  }

  "doFieldsPosition for ticket 2" should "return Some(result)" in {
    Given(s"Input is spec")
    val rule0 = Rule("row", 0, 5, 8, 19)
    val rule1 = Rule("class", 0, 1, 4, 19)
    val rule2 = Rule("seat", 0, 13, 16, 19)
    val rules = List(rule0, rule1, rule2)

    val ticket2 = Ticket(List(15L, 1L, 5L))
    val tickets = List(ticket2)

    When("doFieldsPosition")
    val result = doFieldsPosition(tickets, rules)

    //Map(0 -> Set(rule0, rule2), 1 -> Set(rule0, rule1, rule2), 2 -> Set(rule0, rule1, rule2))
    //Map(0 -> Set(rule0, rule1), 1 -> Set(rule0, rule1, rule2), 2 -> Set(rule0, rule1, rule2))

    Then(s"Result is ")
    result shouldBe Map(
      0 -> Set(rule0, rule1),
      1 -> Set(rule0, rule1, rule2),
      2 -> Set(rule0, rule1, rule2)
    )
  }

  "doFieldsPosition for 2 tickets" should "return Some(result)" in {
    Given(s"Input is spec")
    val rule0 = Rule("row", 0, 5, 8, 19)
    val rule1 = Rule("class", 0, 1, 4, 19)
    val rule2 = Rule("seat", 0, 13, 16, 19)
    val rules = List(rule0, rule1, rule2)

    val ticket1 = Ticket(List(3L, 9L, 18L))
    val ticket2 = Ticket(List(15L, 1L, 5L))
    val tickets = List(ticket1, ticket2)

    When("doFieldsPosition")
    val result = doFieldsPosition(tickets, rules)

    //Map(0 -> Set(rule0, rule2), 1 -> Set(rule0, rule1, rule2), 2 -> Set(rule0, rule1, rule2))
    //Map(0 -> Set(rule0, rule1), 1 -> Set(rule0, rule1, rule2), 2 -> Set(rule0, rule1, rule2))

    Then(s"Result is ")
    result shouldBe Map(
      0 -> Set(rule0),
      1 -> Set(rule1, rule2),
      2 -> Set(rule1, rule2)
    )
  }

  "doFieldsPosition for spec" should "return Some(result)" in {
    Given(s"Input is spec")
    val rule0 = Rule("row", 0, 5, 8, 19)
    val rule1 = Rule("class", 0, 1, 4, 19)
    val rule2 = Rule("seat", 0, 13, 16, 19)
    val rules = List(rule0, rule1, rule2)

    val ticket1 = Ticket(List(3L, 9L, 18L))
    val ticket2 = Ticket(List(15L, 1L, 5L))
    val ticket3 = Ticket(List(5L, 14L, 9L))
    val tickets = List(ticket1, ticket2, ticket3)

    When("doFieldsPosition")
    val result = doFieldsPosition(tickets, rules)

    //Map(0 -> Set(rule0, rule2), 1 -> Set(rule0, rule1, rule2), 2 -> Set(rule0, rule1, rule2))
    //Map(0 -> Set(rule0, rule1), 1 -> Set(rule0, rule1, rule2), 2 -> Set(rule0, rule1, rule2))
    //Map(0 -> Set(rule0, rule1, rule2), 1 -> Set(rule0, rule1), 2 -> Set(rule0, rule1, rule2))

    Then(s"Result is ")
    result shouldBe Map(0 -> Set(rule0), 1 -> Set(rule1), 2 -> Set(rule2))
  }

  "reduce for spec" should "return Some(result)" in {
    Given(s"Input is spec")
    val rule0 = Rule("row", 0, 5, 8, 19)
    val rule1 = Rule("class", 0, 1, 4, 19)
    val rule2 = Rule("seat", 0, 13, 16, 19)
    val rules = List(rule0, rule1, rule2)

    val first = rules.indices.map(i => i -> rules.toSet).toMap
    val ticket1 = Ticket(List(3L, 9L, 18L))
    val r1 =
      List(ticket1).flatMap(doFieldsPosition(_, rules)).foldLeft(first)(reduce)
    r1 shouldBe Map(
      0 -> Set(rule0, rule2),
      1 -> Set(rule0, rule1, rule2),
      2 -> Set(rule0, rule1, rule2)
    )

    val ticket2 = Ticket(List(15L, 1L, 5L))
    val r2 =
      List(ticket2).flatMap(doFieldsPosition(_, rules)).foldLeft(r1)(reduce)
    r2 shouldBe Map(
      0 -> Set(rule0),
      1 -> Set(rule1, rule2),
      2 -> Set(rule1, rule2)
    )

    val ticket3 = Ticket(List(5L, 14L, 9L))
    val r3 =
      List(ticket3).flatMap(doFieldsPosition(_, rules)).foldLeft(r2)(reduce)
    r3 shouldBe Map(0 -> Set(rule0), 1 -> Set(rule1), 2 -> Set(rule2))
  }

  "fieldsPosition for puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When("fieldsPosition")
    val result = fieldsPosition(puzzleInputs)

    Then(s"Result is 998358379943")
    result shouldBe 998358379943L
  }

}
