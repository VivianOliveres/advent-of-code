package com.kensai.aoc.aoc2024

object Day13 {

  // TODO: generalize?
  case class Point2DL(x: Long, y: Long) {
    def *(scalar: Long): Point2DL =
      Point2DL(this.x * scalar, this.y * scalar)
    def +(other: Point2DL): Point2DL =
      Point2DL(this.x + other.x, this.y + other.y)
  }

  case class Machine(a: Point2DL, b: Point2DL, prize: Point2DL)

  private val aRegex     = """Button A: X\+(\d+), Y\+(\d+)""".r
  private val bRegex     = """Button B: X\+(\d+), Y\+(\d+)""".r
  private val prizeRegex = """Prize: X=(\d+), Y=(\d+)""".r

  def parse(rawInput: String): Seq[Machine] = {
    val parts = rawInput.split("\n\n").toSeq
    parts.map(_.split("\n").toSeq).map { subSeq =>
      val a = subSeq.head match {
        case aRegex(x, y) => Point2DL(x.toLong, y.toLong)
        case _            => throw new IllegalArgumentException(s"Should not happen")
      }
      val b = subSeq(1) match {
        case bRegex(x, y) => Point2DL(x.toLong, y.toLong)
        case _            => throw new IllegalArgumentException(s"Should not happen")
      }
      val prize = subSeq(2) match {
        case prizeRegex(x, y) => Point2DL(x.toLong, y.toLong)
        case _                => throw new IllegalArgumentException(s"Should not happen")
      }
      Machine(a, b, prize)
    }
  }

  def parse2(rawInput: String): Seq[Machine] =
    parse(rawInput)
      .map(m => m.copy(prize = m.prize.copy(m.prize.x + 10000000000000L, m.prize.y + 10000000000000L)))

  def win(machines: Seq[Machine], filter: Option[Long] = Some(100L)): Long =
    machines
      .flatMap(win(_, filter))
      .map { case (aCount, bCount) => aCount * 3L + bCount }
      .sum

  def win(machine: Machine, filter: Option[Long]): Option[(Long, Long)] = {
    val det = machine.a.x * machine.b.y - machine.b.x * machine.a.y
    if (det == 0)
      None
    else {
      val x = (machine.prize.x * machine.b.y - machine.prize.y * machine.b.x) / det
      val y = (machine.a.x * machine.prize.y - machine.a.y * machine.prize.x) / det
      if (filter.isDefined && (x > filter.get || y > filter.get))
        None
      else if (machine.a * x + machine.b * y == machine.prize)
        Some((x, y))
      else
        None
    }
  }

}
