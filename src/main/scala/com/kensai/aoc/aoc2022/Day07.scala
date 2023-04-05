package com.kensai.aoc.aoc2022

import scala.annotation.tailrec

object Day07 {

  sealed trait RawNode
  case class FileNode(name: String, size: Int)                        extends RawNode
  case class DirectoryNode(name: String, child: Seq[RawNode] = Seq()) extends RawNode

  def parse(lines: Seq[String]): DirectoryNode =
    doParse(DirectoryNode("/"), Seq("/"), lines.tail)

  private val cdRootRegex   = """\$ cd \/""".r
  private val cdParentRegex = """\$ cd \.\.""".r
  private val cdChildRegex  = """\$ cd (\w+)""".r

  private val lsRegex   = """\$ ls""".r
  private val dirRegex  = """dir (.+)""".r
  private val fileRegex = """(\d+) (.+)""".r

  @tailrec
  private def doParse(root: DirectoryNode, currentPath: Seq[String], remainingLines: Seq[String]): DirectoryNode =
    if (remainingLines.isEmpty)
      root
    else {
      remainingLines.head match {
        case cdRootRegex() =>
          doParse(root, Seq(root.name), remainingLines.tail)
        case cdParentRegex() =>
          doParse(root, currentPath.dropRight(1), remainingLines.tail)
        case cdChildRegex(child) =>
          doParse(root, currentPath :+ child, remainingLines.tail)
        case lsRegex() =>
          val lsOutput = remainingLines.tail.takeWhile(str => !str.startsWith("$"))
          val nodesToInsert = lsOutput.map {
            case dirRegex(dirName)            => DirectoryNode(dirName)
            case fileRegex(sizeStr, fileName) => FileNode(fileName, sizeStr.toInt)
            case str                          => throw new IllegalArgumentException(s"Invalid ls output [$str]")
          }
          val newRoot = insert(root, currentPath.tail, nodesToInsert)
          doParse(newRoot, currentPath, remainingLines.drop(1 + lsOutput.size))
        case str => throw new IllegalArgumentException(s"Invalid instruction [$str]")
      }
    }

  private def insert(root: DirectoryNode, currentPath: Seq[String], nodesToInsert: Seq[RawNode]): DirectoryNode = currentPath match {
    case Nil => root.copy(child = nodesToInsert)
    case head :: tail =>
      root.copy(child = root.child.map {
        case d @ DirectoryNode(name, _) if name == head =>
          insert(d, tail, nodesToInsert)
        case other =>
          other
      })
    case _ => throw new IllegalArgumentException(s"Should not happen")
  }

  case class SizedDirectoryNode(name: String, size: Int, children: Seq[SizedDirectoryNode] = Seq())
  def computeDirectorySizes(root: DirectoryNode): SizedDirectoryNode = {
    val (size, newChildren) = root.child
      .map {
        case FileNode(_, size) =>
          (size, None)
        case d @ DirectoryNode(_, _) =>
          val sizedDirectoryNode = computeDirectorySizes(d)
          (sizedDirectoryNode.size, Some(sizedDirectoryNode))
      }
      .foldLeft((0, Seq.empty[SizedDirectoryNode])) { case ((sizeAcc, accChildren), (size, maybeChild)) =>
        (sizeAcc + size, maybeChild.map(accChildren :+ _).getOrElse(accChildren))
      }
    SizedDirectoryNode(root.name, size, newChildren)
  }

  def sumSizesBellow(root: SizedDirectoryNode, limit: Int): Int = {
    val results = doSumSizesBellow(root, limit)
    results.map(_._2).sum
  }

  private def doSumSizesBellow(root: SizedDirectoryNode, limit: Int): Seq[(String, Int)] = {
    val childrenResults = root.children.flatMap(dir => doSumSizesBellow(dir, limit))
    if (root.size <= limit)
      childrenResults :+ (root.name, root.size)
    else
      childrenResults
  }

  def smallestDirToDelete(totalSpace: Int, spaceNeeded: Int, root: SizedDirectoryNode): Int = {
    val nodesAndSize   = doSumSizesBellow(root, Integer.MAX_VALUE)
    val availableSpace = totalSpace - root.size
    val spaceToClear   = spaceNeeded - availableSpace
    val bigEnoughDirectories = nodesAndSize
      .map { case (name, size) => (name, size, size - spaceToClear) }
      .filter(_._3 >= 0) // ie. size >= spaceToClear
      .sortBy(_._3)
    bigEnoughDirectories.head._2
  }

}
