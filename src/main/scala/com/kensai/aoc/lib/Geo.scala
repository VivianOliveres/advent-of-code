package com.kensai.aoc.lib

object Geo {

  case class Point2D(x: Int, y: Int) {
    def min(that: Point2D): Point2D =
      Point2D(x min that.x, y min that.y)
    def max(that: Point2D): Point2D =
      Point2D(x max that.x, y max that.y)
    def <=(that: Point2D): Boolean =
      x <= that.x && y <= that.y

    def minusX(value: Int): Point2D =
      Point2D(x - value, y)
    def minusY(value: Int): Point2D =
      Point2D(x, y - value)
    def plusX(value: Int): Point2D =
      Point2D(x + value, y)
    def plusY(value: Int): Point2D =
      Point2D(x, y + value)

    def +(other: Point2D): Point2D =
      Point2D(this.x + other.x, this.y + other.y)
    def -(other: Point2D): Point2D =
      Point2D(this.x - other.x, this.y - other.y)
    def *(scalar: Int): Point2D =
      Point2D(this.x * scalar, this.y * scalar)

    def nextPoints(): Seq[Point2D] = Seq(
      Point2D(x + 1, y),
      Point2D(x - 1, y),
      Point2D(x, y + 1),
      Point2D(x, y - 1)
    )

    def in(maxX: Int, maxY: Int): Boolean =
      x >= 0 && y >= 0 && x <= maxX && y <= maxY

    def prettyPrint: String = s"($x, $y)"
  }
  case class Distance2D(x: Int, y: Int)
  case class Cube2D(min: Point2D, max: Point2D) {
    def intersect(that: Cube2D): Option[Cube2D] = {
      val intersectMin = min max that.min
      val intersectMax = max min that.max
      if (intersectMin <= intersectMax)
        Some(Cube2D(intersectMin, intersectMax))
      else
        None
    }
    def iterator: Iterator[Point2D] =
      for {
        x <- (min.x to max.x).iterator
        y <- (min.y to max.y).iterator
      } yield Point2D(x, y)
    def size: Int = iterator.size
  }
  object Cube2D {
    def apply(minX: Int, maxX: Int, minY: Int, maxY: Int): Cube2D =
      Cube2D(Point2D(minX, minY), Point2D(maxX, maxY))
  }

  case class Point3D(x: Int, y: Int, z: Int) {
    def min(that: Point3D): Point3D =
      Point3D(x min that.x, y min that.y, z min that.z)
    def max(that: Point3D): Point3D =
      Point3D(x max that.x, y max that.y, z max that.z)
    def <=(that: Point3D): Boolean =
      x <= that.x && y <= that.y && z <= that.z
  }
  case class Distance3D(x: Int, y: Int, z: Int)
  case class Cube3D(min: Point3D, max: Point3D) {
    def intersect(that: Cube3D): Option[Cube3D] = {
      val intersectMin = min max that.min
      val intersectMax = max min that.max
      if (intersectMin <= intersectMax)
        Some(Cube3D(intersectMin, intersectMax))
      else
        None
    }
    def iterator: Iterator[Point3D] =
      for {
        x <- (min.x to max.x).iterator
        y <- (min.y to max.y).iterator
        z <- (min.z to max.z).iterator
      } yield Point3D(x, y, z)
    def size: Int = iterator.size
  }
  object Cube3D {
    def apply(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int): Cube3D = {
      if (minX > maxX)
        throw new IllegalArgumentException(s"Invalid X coordinates: minX[$minX] maxX[$maxX]")
      if (minY > maxY)
        throw new IllegalArgumentException(s"Invalid X coordinates: minY[$minY] maxY[$maxY]")
      if (minZ > maxZ)
        throw new IllegalArgumentException(s"Invalid X coordinates: minZ[$minZ] maxZ[$maxZ]")
      Cube3D(Point3D(minX, minY, minZ), Point3D(maxX, maxY, maxZ))
    }
  }

}
