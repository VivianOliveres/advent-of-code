package com.kensai.aoc.aoc2021

object Day18 {

  sealed trait Node
  case class Leaf(value: Int) extends Node {
    override def toString: String = value.toString
  }
  case class Parent(left: Node, right: Node) extends Node {
    override def toString: String = s"[$left,$right]"
  }
  case object Parent {
    def apply(left: Int, right: Int): Parent = Parent(Leaf(left), Leaf(right))
    def apply(left: Int, rightNode: Node): Parent =
      Parent(Leaf(left), rightNode)
    def apply(leftNode: Node, right: Int): Parent =
      Parent(leftNode, Leaf(right))
  }

  /** Parse a single line into a Node instance.
    */
  def parse(line: String): Node = {
    val (strHead, strTail) = (line.head, line.tail)
    strHead match {
      case '[' =>
        val (leftPart, rightPart) = doParseParent(strTail)
        val leftNode = parse(leftPart)
        val rightNode = parse(rightPart)
        Parent(leftNode, rightNode)
      case _ => Leaf(strHead.asDigit)
    }
  }

  private def doParseParent(remainingLine: String): (String, String) = {
    val cleanedLine =
      remainingLine.substring(0, remainingLine.length - 1) // skip last bracket
    val (indexComa, _) = cleanedLine.zipWithIndex.foldLeft((-1, 0)) {
      case ((indexComa, countOpenBracket), (char, index)) =>
        if (char == ',' && countOpenBracket == 0) (index, 0)
        else if (char == '[') (indexComa, countOpenBracket + 1)
        else if (char == ']') (indexComa, countOpenBracket - 1)
        else (indexComa, countOpenBracket)
    }
    val leftPart = cleanedLine.substring(0, indexComa)
    val rightPart = cleanedLine.substring(indexComa + 1)
    (leftPart, rightPart)
  }

  /** Addition all nodes sequentially.
    */
  def addAll(nodes: Seq[Node]): Node =
    nodes.tail.foldLeft(nodes.head) { case (result, candidate) =>
      add(result, candidate)
    }

  def add(node1: Node, node2: Node): Node =
    explodeLoop(Parent(node1, node2))

  /** Loop that will explode until the result does not change. Then it will split.
    * If the split does not produce changes, then it returns the result. Else it iterate recursively.
    */
  private def explodeLoop(node: Node): Node = doExplode(node, 0) match {
    case ExplodeNothing         => splitLoop(node)
    case Exploded(_, result, _) => explodeLoop(result)
  }

  /** Split the node and loop on explode if it changes one of the nodes.
    */
  private def splitLoop(node: Node): Node = doSplit(node) match {
    case Some(result) => explodeLoop(result)
    case None         => node
  }

  /** Single Explode action.
    */
  def explode(root: Node): Node = doExplode(root, 0) match {
    case ExplodeNothing         => root
    case Exploded(_, result, _) => result
  }

  sealed trait ExplodeResult
  case object ExplodeNothing extends ExplodeResult
  case class Exploded(
      maybeLeftRest: Option[Int],
      result: Node,
      maybeRightRest: Option[Int]
  ) extends ExplodeResult

  private def doExplode(node: Node, depth: Int): ExplodeResult = node match {
    case Parent(Leaf(left), Leaf(right)) if depth == 4 =>
      Exploded(Some(left), Leaf(0), Some(right))
    case Leaf(_) => ExplodeNothing
    case Parent(leftNode, rightNode) =>
      doExplodeParent(depth, leftNode, rightNode)
  }

  private def doExplodeParent(
      depth: Int,
      leftNode: Node,
      rightNode: Node
  ): ExplodeResult = {
    val leftResult = doExplode(leftNode, depth + 1)
    leftResult match {
      case ExplodeNothing =>
        doExplodeParentRight(depth, leftNode, rightNode)
      case Exploded(None, result, None) =>
        Exploded(None, Parent(result, rightNode), None)
      case Exploded(Some(leftLeft), leftNode, Some(leftRight)) =>
        val (addRightNode, rightRest) = addLeft(rightNode, leftRight)
        Exploded(Some(leftLeft), Parent(leftNode, addRightNode), rightRest)
      case Exploded(Some(leftLeft), result, None) =>
        Exploded(Some(leftLeft), Parent(result, rightNode), None)
      case Exploded(None, leftNode, Some(leftRight)) =>
        val (addRightNode, rightRest) = addLeft(rightNode, leftRight)
        Exploded(None, Parent(leftNode, addRightNode), rightRest)
      case e =>
        println(e)
        ???
    }
  }

  private def doExplodeParentRight(
      depth: Int,
      leftNode: Node,
      rightNode: Node
  ): ExplodeResult = {
    val rightResult = doExplode(rightNode, depth + 1)
    rightResult match {
      case ExplodeNothing =>
        ExplodeNothing
      case Exploded(None, result, None) =>
        Exploded(None, Parent(leftNode, result), None)
      case Exploded(Some(rightLeft), result, None) =>
        val (addLeftNode, leftRest) = addRight(leftNode, rightLeft)
        Exploded(leftRest, Parent(addLeftNode, result), None)
      case Exploded(None, result, Some(rightRight)) =>
        Exploded(None, Parent(leftNode, result), Some(rightRight))
      case Exploded(Some(rightLeft), result, Some(rightRight)) =>
        val (addLeftNode, leftRest) = addRight(leftNode, rightLeft)
        Exploded(
          leftRest,
          Parent(addLeftNode, result),
          Some(rightRight)
        )
      case e =>
        println(e)
        ???
    }
  }

  /**
    * Add this value to first left Leaf node.
    */
  private def addLeft(rightNode: Node, value: Int): (Node, Option[Int]) =
    rightNode match {
      case Leaf(v) => (Leaf(v + value), None)
      case Parent(left, rightNode) =>
        val (result, rest) = addLeft(left, value)
        (Parent(result, rightNode), rest)
    }

  /**
    * Add this value to first right Leaf node.
    */
  private def addRight(leftNode: Node, value: Int): (Node, Option[Int]) =
    leftNode match {
      case Leaf(v) => (Leaf(v + value), None)
      case Parent(left, right) =>
        val (result, rest) = addRight(right, value)
        (Parent(left, result), rest)
    }

  /**
    * Split the first (left order) Leaf node wich value is >= 10.
    */
  def split(root: Node): Node =
    doSplit(root).getOrElse(root)

  private def doSplit(node: Node): Option[Node] = node match {
    case Leaf(value) if value >= 10 =>
      val d = value / 2.0
      Some(Parent(d.toInt, math.ceil(d).toInt))
    case Leaf(_) => None
    case Parent(left, right) =>
      val leftResult = doSplit(left)
      leftResult
        .map(n => Parent(n, right))
        .orElse(doSplit(right).map(r => Parent(left, r)))

  }

  def computeMagnitude(node: Node): Long = node match {
    case Leaf(value) => value.toLong
    case Parent(left, right) =>
      computeMagnitude(left) * 3 + computeMagnitude(right) * 2
  }

  /**
    * Find the two numbers from the list that, after an addition, produces the highest magnitude.
    */
  def computeLargestMagnitude(nodes: Seq[Node]): Long =
    nodes.combinations(2).foldLeft(0L) { case (max, combination) =>
      math.max(max, computeMagnitude(addAll(combination)))
    }
}
