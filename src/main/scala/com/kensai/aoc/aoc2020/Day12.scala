package com.kensai.aoc.aoc2020

import enumeratum._

object Day12 {

  sealed trait Direction extends EnumEntry
  object Direction extends Enum[Direction] {
    val values: IndexedSeq[Direction] = findValues
    case object N extends Direction
    case object E extends Direction
    case object S extends Direction
    case object W extends Direction
  }

  case class Pos(x: Int, y: Int, direction: Direction)

  sealed trait Command {
    def move(pos: Pos): Pos

    protected def rotate(pos: Pos, value: Int): Pos = {
      val index = Direction.indexOf(pos.direction)
      val inc   = value / 90
      val diff  = index + inc
      val newIndex =
        if (diff >= 0) diff                 % Direction.values.size
        else (Direction.values.size + diff) % Direction.values.size
      pos.copy(direction = Direction.values(newIndex))
    }
  }

  case class Forward(value: Int) extends Command {
    import Direction._
    override def move(pos: Pos): Pos = pos.direction match {
      case N => pos.copy(y = pos.y + value)
      case E => pos.copy(x = pos.x + value)
      case W => pos.copy(x = pos.x - value)
      case S => pos.copy(y = pos.y - value)
    }
  }
  case class North(value: Int) extends Command {
    override def move(pos: Pos): Pos =
      pos.copy(y = pos.y + value)
  }
  case class East(value: Int) extends Command {
    override def move(pos: Pos): Pos =
      pos.copy(x = pos.x + value)
  }
  case class West(value: Int) extends Command {
    override def move(pos: Pos): Pos =
      pos.copy(x = pos.x - value)
  }
  case class South(value: Int) extends Command {
    override def move(pos: Pos): Pos =
      pos.copy(y = pos.y - value)
  }
  case class Right(value: Int) extends Command {
    override def move(pos: Pos): Pos =
      this.rotate(pos, value)
  }
  case class Left(value: Int) extends Command {
    override def move(pos: Pos): Pos =
      this.rotate(pos, -value)
  }

  private val rowRegex = """([A-Z])(\d+)""".r

  def parse(inputs: List[String]): List[Command] =
    inputs
      .collect {
        case str if str.nonEmpty => str.trim
      }
      .map {
        case rowRegex(name, value) => (name, value.toInt)
        case something =>
          throw new RuntimeException(s"Unknown input [$something]")
      }
      .map {
        case ("F", value) => Forward(value)
        case ("N", value) => North(value)
        case ("E", value) => East(value)
        case ("W", value) => West(value)
        case ("S", value) => South(value)
        case ("R", value) => Right(value)
        case ("L", value) => Left(value)
        case something =>
          throw new RuntimeException(s"Unknown direction [$something]")
      }

  def move(pos: Pos, inputs: List[Command]): Pos =
    inputs.foldLeft(pos) { case (previous, command) => command.move(previous) }

  def computeManhattanDistance(pos: Pos, inputs: List[String]): Long = {
    val commands = parse(inputs)
    val lastPos  = move(pos, commands)
    manhattanDistance(lastPos).toLong
  }

  private def manhattanDistance(pos: Pos): Int =
    math.abs(pos.x) + math.abs(pos.y)

  private def moveTo(pos: Pos, waypoint: Pos, value: Int): Pos = {
    val moveEast  = East(waypoint.x)
    val moveNorth = North(waypoint.y)
    (1 to value).foldLeft(pos) { case (newPOs, _) =>
      moveNorth.move(moveEast.move(newPOs))
    }
  }

  private def rotate(waypoint: Pos, value: Int): Pos = value match {
    case 90  => waypoint.copy(x = waypoint.y, y = -waypoint.x)
    case 180 => waypoint.copy(x = -waypoint.x, y = -waypoint.y)
    case 270 => waypoint.copy(x = -waypoint.y, y = waypoint.x)
    case 360 => waypoint
  }

  def doMove2(pos: Pos, waypoint: Pos, command: Command): (Pos, Pos) =
    command match {
      case Forward(value) => (moveTo(pos, waypoint, value), waypoint)
      case Right(value)   => (pos, rotate(waypoint, value))
      case Left(value)    => (pos, rotate(waypoint, 360 - value))
      case o              => (pos, o.move(waypoint))
    }

  private def move2(pos: Pos, waypoint: Pos, commands: List[Command]): (Pos, Pos) =
    commands.foldLeft((pos, waypoint)) { case (tuple, command) =>
      doMove2(tuple._1, tuple._2, command)
    }

  def computeManhattanDistance2(
      pos: Pos,
      waypoint: Pos,
      inputs: List[String]
    ): Int = {
    val commands     = parse(inputs)
    val (lastPos, _) = move2(pos, waypoint, commands)
    manhattanDistance(lastPos)
  }

}
