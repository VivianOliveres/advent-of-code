package com.kensai.aoc.aoc2021

import scala.annotation.tailrec

object Day11 {

  case class OctopusPos(x: Int, y: Int)
  case class Octopus(pos: OctopusPos, energy: Int) {
    def increaseEnergy: Octopus =
      Octopus(pos, energy + 1)
  }

  case class OctopusMatrix(octopos: Map[OctopusPos, Octopus]) {

    def debug(): Unit = {
      (0 to 9).foreach{y =>
        (0 to 9).foreach{x =>
          print(octopos(OctopusPos(x, y)).energy.toString)
        }
        println(s"")
      }
    }

    /**
      * Add 1 energy to each octopus
      */
    def increaseEnergy: OctopusMatrix = {
      OctopusMatrix(
        octopos.map { case (pos, octo) => (pos, octo.increaseEnergy) }
      )
    }

    /**
      * Add 1 energy to a specific octopus
      */
    def increaseEnergy(pos: OctopusPos): OctopusMatrix = {
      val updatedOctopus = octopos(pos).increaseEnergy
      OctopusMatrix(
        octopos + (updatedOctopus.pos -> updatedOctopus)
      )
    }

    /**
      * Return all the positions where an octopus has at least 10 energy and so should flash.
      */
    def posToFlash: Seq[OctopusPos]=
      octopos.values.filter(_.energy > 9).map(_.pos).toSeq

    /**
      * Set the energy to 0 for all octopus that have at least 10 energy.
      */
    def reset: (Int, OctopusMatrix) = {
      val (count, map) = octopos.foldLeft((0, Map.empty[OctopusPos, Octopus])) { case (acc, (key -> octo)) =>
        if (octo.energy > 9)
          (acc._1 + 1, acc._2 + (key -> octo.copy(energy = 0)))
        else (acc._1, acc._2 + (key -> octo))
      }
      (count, OctopusMatrix(map))
    }

    def apply(pos: OctopusPos): Octopus =
      octopos(pos)

    /**
      * Increase energy of each neighbors octopus. If one of them flash (ie energy == 10) then it is returned.
      * The test to energy==10 allows to not flash multiple times an octopus.
      */
    def flashNeighbors(pos: OctopusPos): (OctopusMatrix, Seq[OctopusPos]) = {
      val neighborsPos = for {
        x <- pos.x - 1 to pos.x + 1
        y <- pos.y - 1 to pos.y + 1
        if (x != pos.x || y != pos.y) && x >= 0 && y >= 0 && x < 10 && y < 10
      } yield OctopusPos(x, y)

      neighborsPos.foldLeft((this, Seq.empty[OctopusPos])) {
        case (acc, posToIncrease) =>
          val updatedMatrix: OctopusMatrix =
            acc._1.increaseEnergy(posToIncrease)
          if (updatedMatrix(posToIncrease).energy == 10)
            (updatedMatrix, acc._2 :+ posToIncrease)
          else
            (updatedMatrix, acc._2)
      }
    }
  }

  private def parse(lines: Seq[String]): OctopusMatrix = {
    val octopuses = lines
      .filterNot(_.isEmpty)
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.toCharArray.map(_.toString.toInt).zipWithIndex.map {
          case (energy, x) => Octopus(OctopusPos(x, y), energy)
        }
      }
      .groupBy(_.pos)
      .map(tuple => (tuple._1, tuple._2.head))
    OctopusMatrix(octopuses)
  }

  /**
    * For each step, count the number of flashes that happen. Then sum it.
    */
  def countFlashes(lines: Seq[String], steps: Int): Int = {
    val matrix = parse(lines)
    val (countFlashes, _): (Int, OctopusMatrix) =
      (0 until steps).foldLeft((0, matrix)) { case (acc, _) =>
        val incMatrix = acc._2.increaseEnergy
        val flashedMatrix = doIncreaseNeighborsEnergy(incMatrix, incMatrix.posToFlash)
        val (count, cleanedMatrix) = flashedMatrix.reset
        (acc._1 + count, cleanedMatrix)
      }

    countFlashes
  }

  @tailrec
  private def doIncreaseNeighborsEnergy(matrix: OctopusMatrix, positionsToFlash: Seq[OctopusPos]): OctopusMatrix = {
    if (positionsToFlash.isEmpty)
      matrix
    else {
      val pos = positionsToFlash.head
      val (newMatrix, otherPositionsThatFlash) = matrix.flashNeighbors(pos)
      doIncreaseNeighborsEnergy(newMatrix, positionsToFlash.tail ++ otherPositionsThatFlash)
    }
  }

  /**
    * Return the first step when every octopus flash in the same time.
    */
  def computeFirstGlobalFlash(lines: Seq[String]): Int = {
    val matrix = parse(lines)
    doComputeFirstGlobalFlash(0, matrix)
  }

  @tailrec
  private def doComputeFirstGlobalFlash(step: Int, matrix: OctopusMatrix): Int = {
    val incMatrix = matrix.increaseEnergy
    val flashedMatrix = doIncreaseNeighborsEnergy(incMatrix, incMatrix.posToFlash)
    val (count, cleanedMatrix) = flashedMatrix.reset
    if (count == 100)
      step + 1
    else
      doComputeFirstGlobalFlash(step + 1, cleanedMatrix)
  }
}
