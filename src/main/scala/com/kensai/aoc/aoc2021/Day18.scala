package com.kensai.aoc.aoc2021

object Day18 {

  sealed trait Node
  case class Leaf(value: Int) extends Node
  case class Parent(left: Node, right: Node) extends Node
  case object Parent {
    def apply(left: Int, right: Int): Parent = Parent(Leaf(left), Leaf(right))
  }

  def parse(line: String): Node = {
    doParseNode(line)
  }

  private def doParseNode(remainingLine: String):Node = {
    val (strHead, strTail) = (remainingLine.head, remainingLine.tail)
    strHead match {
      case '[' =>
        val (leftPart, rightPart) = doParseParent(strTail)
        val leftNode = doParseNode(leftPart)
        val rightNode = doParseNode(rightPart)
        Parent(leftNode, rightNode)
      case _ => Leaf(strHead.asDigit)
    }
  }

  private def doParseParent(remainingLine: String): (String, String) = {
    val cleanedLine = remainingLine.substring(0, remainingLine.length - 1) // skip last bracket
    val (indexComa, _) = cleanedLine.zipWithIndex.foldLeft((-1, 0)){case ((indexComa, countOpenBracket), (char, index)) =>
      if (char == ',' && countOpenBracket == 0) (index, 0)
      else if (char == '[') (indexComa, countOpenBracket + 1)
      else if (char == ']') (indexComa, countOpenBracket - 1)
      else (indexComa, countOpenBracket)
    }
    val leftPart = cleanedLine.substring(0, indexComa)
    val rightPart = cleanedLine.substring(indexComa + 1)
    (leftPart, rightPart)
  }

  def add(node1: Node, node2: Node): Node = {
    Parent(node1, node2)
  }

  def explode(root: Node): Option[(Int, Node, Int)] = {
    val maybeNodeToExplode = findNodeToExplode(root, 0)
    println(maybeNodeToExplode)
    maybeNodeToExplode.map{ case (nodeToExplode, path) =>
      val left = findLeftNode(root, nodeToExplode)
      val right = findRightNode(root, nodeToExplode)
    }

    ???
  }

  private def doExplode(node: Node, depth: Int): Option[(Int, Node, Int)] = {
    node match {
      case Parent(Leaf(left), Leaf(right)) if depth == 4 => Some((left, node, right))
      case Leaf(_) => None
      case Parent(leftNode, rightNode) => // 3
        val leftResult = doExplode(leftNode, depth + 1)
          .map{case (leftValue, node, rightValue) =>
          (0, Parent(Leaf(0), ), 0)
        }
        val rightResult = doExplode(rightNode, depth + 1)
          .map{case (leftValue, node, rightValue) =>
            Parent(Leaf(0), )
          }
          .orElse(doExplode(rightNode, depth + 1))
        result.map{case (leftValue, node, rightValue) =>

        }
      None
    }
  }

  private def findLeftNode(root: Node, nodeToExplode: Node): Option[Node] = {
    root match {
      case Parent(left, right) if left == nodeToExplode => Some(node)
      case Leaf(_) => None
      case Parent(left, right) => findNodeToExplode(left, currentDepth + 1).orElse(findNodeToExplode(right, currentDepth + 1))
    }
  }
}
