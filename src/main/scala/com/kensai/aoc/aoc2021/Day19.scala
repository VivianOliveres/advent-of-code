package com.kensai.aoc.aoc2021

import scala.annotation.tailrec
import scala.math.abs

object Day19 {

  case class Distance(x: Int, y: Int, z: Int) {
    def toPoint: Point = Point(x, y, z)
  }

  case class Point(x: Int, y: Int, z: Int) {

    def rotate(index: Int): Point = generateRotationPoints(index)

    def generateRotationPoints: Seq[Point] = Seq(
      Point(x, y, z),
      Point(-y, x, z),
      Point(-x, -y, z),
      Point(y, -x, z),
      Point(-x, y, -z),
      Point(y, x, -z),
      Point(x, -y, -z),
      Point(-y, -x, -z),
      Point(-z, y, x),
      Point(-z, x, -y),
      Point(-z, -y, -x),
      Point(-z, -x, y),
      Point(z, y, -x),
      Point(z, x, y),
      Point(z, -y, x),
      Point(z, -x, -y),
      Point(x, -z, y),
      Point(-y, -z, x),
      Point(-x, -z, -y),
      Point(y, -z, -x),
      Point(x, z, -y),
      Point(-y, z, -x),
      Point(-x, z, y),
      Point(y, z, x)
    )

    def distance(to: Point): Distance =
      Distance(to.x - x, to.y - y, to.z - z)

    def translation(to: Point): Distance =
      Distance(x - to.x, y - to.y, z - to.z)

    def translate(d: Distance): Point =
      Point(x + d.x, y + d.y, z + d.z)

    def manhattanDistance(to: Point): Distance =
      Distance(abs(to.x - x), abs(to.y - y), abs(to.z - z))
  }

  case class Scanner(id: Int, points: Seq[Point])

  case class Inputs(scanners: Seq[Scanner])

  private val scannerRegex = """--- scanner (\d+) ---""".r
  private val pointRegex   = """(-?\d+),(-?\d+),(-?\d+)""".r
  def parse(fileContent: String): Inputs = {
    val splitAreas = fileContent.split("\n\n")
    val scanners = splitAreas.map { area =>
      val lines = area.split("\n")
      val scannerId = lines.head match {
        case scannerRegex(idStr) => idStr.toInt
        case _ =>
          throw new IllegalArgumentException(
            s"Invalid line for scanner header [${area.head}]"
          )
      }
      val points = lines.tail.map {
        case pointRegex(x, y, z) => Point(x.toInt, y.toInt, z.toInt)
        case line =>
          throw new IllegalArgumentException(s"Invalid line for point [$line]")
      }.toSeq
      Scanner(scannerId, points)
    }.toSeq
    Inputs(scanners)
  }

  // Parse
  // For each scanner compute the distance of each points to each other points
  // For scanners 1 to n, compute also the other possibilities by rotation
  // foldLeft(scanner0) on scanner 1 to n
  //   find first scanner that share sames distances than scanner 0 for at least 12 points
  //   updates its positions according to scanner 0
  //   put them into accumulator
  // Do the same for other scanners (find + update)
  // Then for each new scanner, continue (find + update) until there is no scanner empty
  // Export all beacons in map and return the size (ie, count of beacons with a relative position to scanner 0)

  def compute(inputs: Inputs): Long = {
    val results = doCompute(inputs)
    results.currentPoints.size.toLong
  }

  private def doCompute(inputs: Inputs): Acc = {
    val scanner0    = inputs.scanners.head
    val s0Distances = computeDistancesFromTo(scanner0.points.toSet)
    val acc = Acc(
      scanner0.points.toSet,
      s0Distances,
      inputs.scanners.tail,
      Seq((0, Point(0, 0, 0)))
    )
    doMergeScanners(acc)
  }

  def computeDistancesFromTo(
      newPoints: Set[Point],
      oldPoints: Set[Point]
    ): Map[Distance, (Point, Point)] =
    (for {
      newPoint <- newPoints
      if !oldPoints.contains(newPoint)
      oldPoint <- oldPoints
    } yield newPoint.distance(oldPoint) -> (newPoint, oldPoint)).toMap

  def computeDistancesFromTo(
      points: Set[Point]
    ): Map[Distance, (Point, Point)] =
    computeDistancesFromTo(points.toSeq)

  def computeDistancesFromTo(
      points: Seq[Point]
    ): Map[Distance, (Point, Point)] =
    points
      .combinations(2)
      .map(seq => (seq.head, seq.tail.head))
      .map { case (from, to) => (from.distance(to), (from, to)) }
      .toMap

  def rotate(points: Seq[Point], index: Int): Seq[Point] =
    points.map(_.rotate(index))

  def matches(
      scanner: Map[Distance, (Point, Point)],
      beacons: Map[Distance, (Point, Point)]
    ): Map[Distance, ((Point, Point), (Point, Point))] =
    for {
      (distBeacon, beaconPoints) <- beacons
      if scanner.contains(distBeacon)
    } yield distBeacon -> (scanner(distBeacon), beaconPoints)

  /** Accumulator that keeps tracks of scanners and points relative to scanner 0.
    */
  case class Acc(
      currentPoints: Set[Point],
      currentDistances: Map[Distance, (Point, Point)],
      availableScanners: Seq[Scanner],
      scannersPositions: Seq[(Int, Point)]) {
    def update(
        pointsToAdd: Set[Point],
        scannerToRemove: Scanner,
        scannerPosition: Point
      ): Acc = {
      val newPointsDistances = computeDistancesFromTo(
        pointsToAdd,
        currentPoints
      ) ++ computeDistancesFromTo(pointsToAdd)
      val newAvailableScanners =
        availableScanners.filterNot(_ == scannerToRemove)
      val newScannersPositions =
        scannersPositions :+ (scannerToRemove.id, scannerPosition)
      Acc(
        currentPoints ++ pointsToAdd,
        newPointsDistances ++ currentDistances,
        newAvailableScanners,
        newScannersPositions
      )
    }
  }

  @tailrec
  def doMergeScanners(acc: Acc): Acc =
    if (acc.availableScanners.isEmpty)
      acc
    else {
      val possibilities = for {
        otherScanner  <- acc.availableScanners
        rotateAttempt <- 0 to 23
      } yield (otherScanner, rotateAttempt)

      val (beaconPoints, otherScanner, results) = possibilities
        .map { case (otherScanner, rotateAttempt) =>
          val beaconPoints = rotate(otherScanner.points, rotateAttempt)
          val s1Distances  = computeDistancesFromTo(beaconPoints)
          val results      = matches(acc.currentDistances, s1Distances)
          (beaconPoints, otherScanner, results)
        }
        .find(_._3.size >= 12)
        .get

      // Find the correct rotation -> relocate points
      val headResult   = results.head._2
      val scannerPoint = headResult._1._1
      val beaconPoint  = headResult._2._1
      val translation  = scannerPoint.translation(beaconPoint)
      val translatedBeaconPoints =
        beaconPoints.map(_.translate(translation)).toSet

      // Aggregate points with ones from scanner
      val r =
        acc.update(translatedBeaconPoints, otherScanner, translation.toPoint)

      doMergeScanners(r)
    }

  def manhattanDistance(inputs: Inputs): Int = {
    val results = doCompute(inputs)
    results.scannersPositions
      .map(_._2)
      .combinations(2)
      .toSeq
      .map { case seq =>
        val (first, second) = (seq.head, seq.tail.head)
        first.manhattanDistance(second)
      }
      .map(d => d.x + d.y + d.z)
      .max
  }
}
