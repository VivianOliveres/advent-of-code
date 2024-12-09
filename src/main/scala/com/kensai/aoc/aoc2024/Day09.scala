package com.kensai.aoc.aoc2024

import scala.annotation.tailrec

object Day09 {

  sealed trait Id
  case object EmptyId           extends Id
  case class FullId(value: Int) extends Id

  case class Day9Input(blocks: Seq[(Id, Int)])

  def parse(row: String): Day9Input = {
    val toto = row.zipWithIndex
      .map { case (c, index) =>
        if (index % 2 == 0)
          (FullId(index / 2), c.toString.toInt)
        else
          (EmptyId, c.toString.toInt)
      }
    Day9Input(toto)
  }

  def rearrange(input: Day9Input): Seq[(FullId, Int)] =
    doRearrange(
      initials = input.blocks,
      i = 0,
      j = input.blocks.size - 1,
      remainingI = Some(input.blocks.head),
      remainingJ = Some(input.blocks.last),
      maybeLast = None,
      acc = Seq()
    )

  @tailrec
  private def doRearrange(
      initials: Seq[(Id, Int)],
      i: Int,
      j: Int,
      remainingI: Option[(Id, Int)],
      remainingJ: Option[(Id, Int)],
      maybeLast: Option[(Id, Int)],
      acc: Seq[(FullId, Int)]
    ): Seq[(FullId, Int)] =
    if (i >= j) {
      maybeLast match {
        case Some((EmptyId, _)) =>
          acc
        case Some((FullId(v), c)) =>
          acc :+ (FullId(v), c)
        case None =>
          acc
      }
    } else {
      (remainingI, remainingJ) match {
        case (None, Some(_)) =>
          doRearrange(
            initials = initials,
            i = i + 1,
            j = j,
            remainingI = Some(initials(i + 1)),
            remainingJ = remainingJ,
            maybeLast = remainingJ,
            acc = acc
          )
        case (Some(_), None) =>
          doRearrange(
            initials = initials,
            i = i,
            j = j - 1,
            remainingI = remainingI,
            remainingJ = Some(initials(j - 1)),
            maybeLast = None,
            acc = acc
          )
        case (Some(_), Some((EmptyId, _))) =>
          doRearrange(
            initials = initials,
            i = i,
            j = j - 1,
            remainingI = remainingI,
            remainingJ = Some(initials(j - 1)),
            maybeLast = None,
            acc = acc
          )
        case (Some((FullId(v), count)), Some(_)) =>
          doRearrange(
            initials = initials,
            i = i + 1,
            j = j,
            remainingI = Some(initials(i + 1)),
            remainingJ = remainingJ,
            maybeLast = remainingJ,
            acc = acc :+ (FullId(v), count)
          )
        case (Some((EmptyId, countI)), Some((FullId(vJ), countJ))) =>
          if (countI == countJ)
            doRearrange(
              initials = initials,
              i = i + 1,
              j = j - 1,
              remainingI = Some(initials(i + 1)),
              remainingJ = Some(initials(j - 1)),
              maybeLast = None,
              acc = acc :+ (FullId(vJ), countJ)
            )
          else if (countI > countJ)
            doRearrange(
              initials = initials,
              i = i,
              j = j - 1,
              remainingI = Some((EmptyId, countI - countJ)),
              remainingJ = Some(initials(j - 1)),
              maybeLast = None,
              acc = acc :+ (FullId(vJ), countJ)
            )
          else
            doRearrange(
              initials = initials,
              i = i + 1,
              j = j,
              remainingI = Some(initials(i + 1)),
              remainingJ = Some((FullId(vJ), countJ - countI)),
              maybeLast = Some((FullId(vJ), countJ - countI)),
              acc = acc :+ (FullId(vJ), countI)
            )
        case (None, None) =>
          doRearrange(
            initials = initials,
            i = i,
            j = j,
            remainingI = Some(initials(i)),
            remainingJ = Some(initials(j)),
            maybeLast = None,
            acc = acc
          )
      }
    }

  def checkSum(input: Day9Input): Long =
    rearrange(input)
      .flatMap { case (FullId(value), count) => (0 until count).map(_ => value.toLong) }
      .zipWithIndex
      .foldLeft(0L) { case (acc, (value, index)) => acc + value * index }

  def rearrange3(input: Day9Input): Seq[(Id, Int)] =
    doRearrange3(
      remaining = input.blocks.toVector,
      acc = Vector()
    )

  // Highly inspired from https://github.com/sim642/adventofcode/blob/master/src/main/scala/eu/sim642/adventofcode2024/Day9.scala#L57-L83
  @tailrec
  private def doRearrange3(remaining: Vector[(Id, Int)], acc: Vector[(Id, Int)]): Vector[(Id, Int)] =
    remaining match {
      case xs :+ (toto @ (EmptyId, _)) =>
        doRearrange3(xs, toto +: acc)

      case xs :+ (toto @ (FullId(id), count)) =>
        val index = xs.indexWhere {
          case (EmptyId, emptyCount) => emptyCount >= count
          case (FullId(_), _)        => false
        }
        if (index >= 0) {
          val (before, (EmptyId, emptyCount) +: after) = xs.splitAt(index)
          val replace =
            if (count == emptyCount)
              Vector((FullId(id), count))
            else
              Vector((FullId(id), count)) :+ (EmptyId, emptyCount - count)
          doRearrange3(before ++ replace ++ after, (EmptyId, count) +: acc)
        } else
          doRearrange3(xs, toto +: acc)

      case _ =>
        acc
    }

  def checkSum2(input: Day9Input): Long =
    rearrange3(input)
      .flatMap {
        case (EmptyId, count)       => (0 until count).map(_ => 0L)
        case (FullId(value), count) => (0 until count).map(_ => value.toLong)
      }
      .zipWithIndex
      .foldLeft(0L) { case (acc, (value, index)) => acc + value * index }

}
