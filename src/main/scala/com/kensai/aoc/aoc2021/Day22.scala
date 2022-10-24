package com.kensai.aoc.aoc2021

import com.kensai.aoc.lib.Geo.{Cube3D, Point3D}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{max, min}

object Day22 {

  case class Instruction(isOn: Boolean, cube3D: Cube3D)
  case class Day22Input(instructions: List[Instruction])

  private val onRegex = """(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r
  def parse(rows: Seq[String]): Day22Input = {
    val instructions = rows.map(_.trim).map {
      case onRegex(onOff, minX, maxX, minY, maxY, minZ, maxZ) => Instruction(onOff == "on", Cube3D(minX.toInt, maxX.toInt, minY.toInt, maxY.toInt, minZ.toInt, maxZ.toInt))
      case row => throw new IllegalArgumentException(s"Invalid row [$row]")
    }
    Day22Input(instructions.toList)
  }

  /**
    * Generate all points in a Set and return the size.
    */
  def countCubeOn(input: Day22Input): Int = {
    val cubeons = doApplyInstructions(input.instructions, Set())
    cubeons.size
  }

  @tailrec
  def doApplyInstructions(instructions: List[Instruction], cubeOns: Set[Point3D]): Set[Point3D] =
    instructions match {
      case Nil =>
        cubeOns
      case head :: tail =>
        val newCubeOns = head match {
          case Instruction(true, _) =>
            cubeOns ++ generatePoints(head)
          case Instruction(false, _) =>
            cubeOns -- generatePoints(head)
          case _ =>
            throw new RuntimeException(s"Invalid instruction: $head")
        }
        doApplyInstructions(tail, newCubeOns)
    }

  private def generatePoints(instruction: Instruction): Set[Point3D] =
    (for {
      x <- max(instruction.cube3D.min.x, -50) to min(instruction.cube3D.max.x, 50)
      y <- max(instruction.cube3D.min.y, -50) to min(instruction.cube3D.max.y, 50)
      z <- max(instruction.cube3D.min.z, -50) to min(instruction.cube3D.max.z, 50)
    } yield Point3D(x, y, z))
      .toSet

  /**
    * Generate intersections on cubes
    */
  def countCubeOn2(input: Day22Input): Long =
    doCountCubeOn2(input.instructions)

  private def doCountCubeOn2(instructions: Seq[Instruction]): Long = {
    var cubes: mutable.Seq[Option[Cube3D]] = mutable.Seq()
    instructions.foreach { instr =>
      // Check this cube Instruction with each previously computed cubes
      cubes.zipWithIndex.foreach { case (maybeCube, index) =>
        val cube = maybeCube.get
        if (!filtering(instr, cube)) {
          cubes(index) = None
          if (instr.cube3D.min.x > cube.min.x)
            cubes = cubes :+ Some(Cube3D(cube.min.x, instr.cube3D.min.x - 1, cube.min.y, cube.max.y, cube.min.z, cube.max.z))
          if (instr.cube3D.max.x < cube.max.x)
            cubes = cubes :+ Some(Cube3D(instr.cube3D.max.x + 1, cube.max.x, cube.min.y, cube.max.y, cube.min.z, cube.max.z))
          if (instr.cube3D.min.y > cube.min.y)
            cubes = cubes :+ Some(Cube3D(max(cube.min.x, instr.cube3D.min.x), min(cube.max.x, instr.cube3D.max.x), cube.min.y, instr.cube3D.min.y - 1, cube.min.z, cube.max.z))
          if (instr.cube3D.max.y < cube.max.y)
            cubes = cubes :+ Some(Cube3D(max(cube.min.x, instr.cube3D.min.x), min(cube.max.x, instr.cube3D.max.x), instr.cube3D.max.y + 1, cube.max.y, cube.min.z, cube.max.z))
          if (instr.cube3D.min.z > cube.min.z)
            cubes = cubes :+ Some(Cube3D(max(cube.min.x, instr.cube3D.min.x), min(cube.max.x, instr.cube3D.max.x), max(cube.min.y, instr.cube3D.min.y), min(cube.max.y, instr.cube3D.max.y), cube.min.z, instr.cube3D.min.z - 1))
          if (instr.cube3D.max.z < cube.max.z)
            cubes = cubes :+ Some(Cube3D(max(cube.min.x, instr.cube3D.min.x), min(cube.max.x, instr.cube3D.max.x), max(cube.min.y, instr.cube3D.min.y), min(cube.max.y, instr.cube3D.max.y), instr.cube3D.max.z + 1, cube.max.z))
        }
      }

      // Default for 'on' instructions => add it
      if (instr.isOn)
        cubes = cubes :+ Some(instr.cube3D)

      // Remove useless
      cubes = cubes.filter(_.isDefined)
    }

    // Compute result
    cubes.flatten.foldLeft(0L){case (acc, cube) =>
      acc + (cube.max.x - cube.min.x + 1L) * (cube.max.y - cube.min.y + 1L) * (cube.max.z - cube.min.z + 1L)
    }
  }

    private def filtering(instruction: Instruction, cube: Cube3D): Boolean =
      instruction.cube3D.min.x > cube.max.x || instruction.cube3D.max.x < cube.min.x ||
        instruction.cube3D.min.y > cube.max.y || instruction.cube3D.max.y < cube.min.y ||
        instruction.cube3D.min.z > cube.max.z || instruction.cube3D.max.z < cube.min.z

}
