package com.kensai.aoc.aoc2020

import scala.util.parsing.combinator.JavaTokenParsers

object Day18 extends JavaTokenParsers {

  //TODO: clean

  sealed trait Expr {
    def evaluate(): Long
  }
  case class Num(value: Long) extends Expr {
    override def evaluate(): Long = value
  }
  case class Add(left: Expr, right: Expr) extends Expr {
    override def evaluate(): Long = left.evaluate() + right.evaluate()
  }
  case class Mul(left: Expr, right: Expr) extends Expr {
    override def evaluate(): Long = left.evaluate() * right.evaluate()
  }

  def parse(input: String): Long = {
    def op: Parser[(Expr, Expr) => Expr] = addParser | multParser
    def addParser: Parser[(Expr, Expr) => Expr] = "+" ^^^ Add
    def multParser: Parser[(Expr, Expr) => Expr] = "*" ^^^ Mul
    def valueParser: Parser[Expr] = "\\d+".r ^^ { value => Num(value.toLong) }
    def parenthesisParser: Parser[Expr] = "(" ~> expr <~ ")"
    def leafParser: Parser[Expr] = valueParser | parenthesisParser
    def expr: Parser[Expr] = chainl1(leafParser, op)

    val value = parseAll(expr, input)
    value.get.evaluate()
  }

  def parse2(input: String): Long = {
    def addParser: Parser[(Expr, Expr) => Expr] = "+" ^^^ Add
    def multParser: Parser[(Expr, Expr) => Expr] = "*" ^^^ Mul
    def valueParser: Parser[Expr] = "\\d+".r ^^ { value => Num(value.toLong) }
    def parenthesisParser: Parser[Expr] = "(" ~> expr <~ ")"
    def leafParser: Parser[Expr] = valueParser | parenthesisParser
    def expr: Parser[Expr] = chainl1(chainl1(leafParser, addParser), multParser)

    val value = parseAll(expr, input)
    value.get.evaluate()
  }

  def compute(inputs: List[String]): Long =
    inputs.map(parse).sum

  def compute2(inputs: List[String]): Long =
    inputs.map(parse2).sum
}
