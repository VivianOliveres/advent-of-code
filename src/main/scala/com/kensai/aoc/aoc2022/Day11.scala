package com.kensai.aoc.aoc2022

object Day11 {

  case class Action(update: Long => Long, divideTest: Int, trueSend: Int, falseSend: Int)
  case class Monkey(id: Int, itemsInspected: Long, worryLevels: Seq[Long], action: Action) {
    def addLevel(worryLevel: Long): Monkey =
      this.copy(worryLevels = worryLevels :+ worryLevel)
    def addItemsInspected: Monkey =
      this.copy(itemsInspected = itemsInspected + worryLevels.size, worryLevels = Seq())
  }

  def parse(rawContent: String): Map[Int, Monkey] = {
    val monkeysRaw = rawContent.split("\n\n").toSeq
    monkeysRaw.map(parseMonkey).map(m => (m.id, m)).toMap
  }

  private val monkeyRegex =
    """Monkey (\d):\n  Starting items: (.+)\n  Operation: (.*)\n  Test: divisible by (\d+)\n    If true: throw to monkey (\d+)\n    If false: throw to monkey (\d+)""".r
  private val multRegex = """new = old \* (\d+)""".r
  private val addRegex  = """new = old \+ (\d+)""".r
  private def parseMonkey(rawContent: String): Monkey = rawContent match {
    case monkeyRegex(idStr, itemsStr, operationStr, divideStr, trueSendStr, falseSendStr) =>
      val worryLevels = itemsStr.replace(" ", "").split(",").map(_.toLong).toSeq
      val update = operationStr match {
        case "new = old * old" => old: Long => old * old
        case multRegex(str)    => old: Long => old * str.toLong
        case addRegex(str)     => old: Long => old + str.toLong
        case str               => throw new IllegalArgumentException(s"Can not parse update action [$str]")
      }
      val action = Action(update, divideStr.toInt, trueSendStr.toInt, falseSendStr.toInt)
      Monkey(idStr.toInt, 0, worryLevels, action)
    case str => throw new IllegalArgumentException(s"Cannot parse [$str]")
  }

  def executeRound(divideWorryLevelFn: Long => Long, monkeys: Map[Int, Monkey]): Map[Int, Monkey] =
    (0 until monkeys.size).foldLeft(monkeys) { (acc, id) =>
      val monkey = acc(id)
      val newAcc = monkey.worryLevels
        .map(worryLevel => divideWorryLevelFn(monkey.action.update(worryLevel)))
        .map(newWorryLevel =>
          if (newWorryLevel         % monkey.action.divideTest == 0)
            monkey.action.trueSend -> newWorryLevel
          else
            monkey.action.falseSend -> newWorryLevel
        )
        .foldLeft(acc) { case (updatedAcc, (newMonkey, worryLevelToSend)) =>
          updatedAcc + (newMonkey -> updatedAcc(newMonkey).addLevel(worryLevelToSend))
        }
      newAcc + (monkey.id -> monkey.addItemsInspected)
    }

  def computeMonkeyBusiness(rounds: Int, isPart1: Boolean, monkeys: Map[Int, Monkey]): Long = {
    // All monkey divisors are prime numbers, so the best way to keep Relief "manageable" is to
    // modulo by Greatest Common Divisor (ie multiplication by each prime number)
    val gcd = monkeys.values.map(_.action.divideTest.toLong).foldLeft(1L) { case (acc, div) => acc * div }
    val divideWorryLevelFn: Long => Long =
      if (isPart1) _ / 3
      else _ % gcd
    val results = (1 to rounds).foldLeft(monkeys) { case (acc, _) =>
      executeRound(divideWorryLevelFn, acc)
    }
    val mostActiveMonkeys      = results.values.toSeq.sortWith(_.itemsInspected > _.itemsInspected).take(2)
    val (first :: second :: _) = mostActiveMonkeys.map(_.itemsInspected)
    first * second
  }

}
