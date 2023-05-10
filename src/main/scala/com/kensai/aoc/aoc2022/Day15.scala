package com.kensai.aoc.aoc2022

import com.kensai.aoc.lib.Geo.Point2D

import scala.annotation.tailrec

object Day15 {

  case class Sensor(pos: Point2D, beacon: Point2D)

  private val rowRegex = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
  def parse(lines: Seq[String]): Set[Sensor] =
    lines
      .collect {
        case str if str.nonEmpty => str.trim
      }
      .map {
        case rowRegex(posX, posY, beaconX, beaconY) => Sensor(Point2D(posX.toInt, posY.toInt), Point2D(beaconX.toInt, beaconY.toInt))
        case str                                    => throw new IllegalAccessException(s"Invalid line [$str]")
      }
      .toSet

  case class RangeDay15(minX: Int, maxX: Int) {
    def count: Int =
      math.abs(maxX - minX) + 1
  }

  private def computeRanges(sensors: Set[Sensor], yTarget: Int): Seq[RangeDay15] =
    (for {
      sensor <- sensors
      manDist  = math.abs(sensor.pos.x - sensor.beacon.x) + math.abs(sensor.pos.y - sensor.beacon.y)
      xManDist = manDist - math.abs(sensor.pos.y - yTarget) if xManDist >= 0
    } yield {
      val min = math.min(sensor.pos.x - xManDist, sensor.pos.x + xManDist)
      val max = math.max(sensor.pos.x - xManDist, sensor.pos.x + xManDist)
      RangeDay15(min, max)
    }).toSeq

  private def mergeRanges(ranges: Seq[RangeDay15]): Seq[RangeDay15] =
    ranges.sortBy(_.minX).foldLeft(List.empty[RangeDay15]) {
      case (prev :: tl, current) if current.minX <= prev.maxX + 1 =>
        RangeDay15(prev.minX, math.max(prev.maxX, current.maxX)) :: tl
      case (acc, cur) =>
        cur :: acc
    }

  def countInvalidPositions(sensors: Set[Sensor], yTarget: Int): Int = {
    val ranges = computeRanges(sensors, yTarget)

    val xBeacons = sensors.filter(_.beacon.y == yTarget).map(_.beacon.x)
    val rangesWithoutBeacons = xBeacons.foldLeft(ranges) { case (acc, xBeacon) =>
      acc.flatMap { range =>
        if (range.minX == xBeacon)
          Seq(RangeDay15(xBeacon + 1, range.maxX))
        else if (range.maxX == xBeacon)
          Seq(RangeDay15(range.minX, xBeacon - 1))
        else if (range.minX < xBeacon && range.maxX > xBeacon)
          Seq(RangeDay15(range.minX, xBeacon - 1), RangeDay15(xBeacon + 1, range.maxX))
        else
          Seq(range)
      }
    }

    val allRanges = mergeRanges(rangesWithoutBeacons)
    allRanges
      .map(_.count)
      .sum
  }

  def findValidPosition(sensors: Set[Sensor], yTarget: Int, maxValue: Int): Option[Point2D] = {
    val ranges       = computeRanges(sensors, yTarget)
    val mergedRanges = mergeRanges(ranges)
    mergedRanges
      .find(range => range.minX > 0 || range.maxX < maxValue)
      .map { range =>
        if (range.minX > 0)
          range.minX - 1
        else
          range.maxX + 1
      }
      .map(x => Point2D(x, yTarget))
  }

  def tuningFrequency(sensors: Set[Sensor], maxValue: Int): Long = {
    // TODO: improve performance
    // 18s if we use (0 to maxValue)
    // 7s if (maxValue to 0 by(-1))
    val result = searchValidPositions(sensors, maxValue, maxValue to 0 by -1)
    result.x * 4000000L + result.y
  }

  @tailrec
  private def searchValidPositions(sensors: Set[Sensor], maxValue: Int, remaining: Seq[Int]): Point2D = {
    val result = findValidPosition(sensors, remaining.head, maxValue)
    if (result.isEmpty)
      searchValidPositions(sensors, maxValue, remaining.tail)
    else
      result.get
  }

}
