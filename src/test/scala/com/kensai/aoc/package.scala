package com.kensai

import scala.io.Source
import scala.language.reflectiveCalls

package object aoc {

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def readInputFile(path: String): String =
    using(Source.fromFile(path))(_.mkString)

  def readInputLines(path: String): List[String] =
    using(Source.fromFile(path))(_.getLines().toList)

  def readLongInputLines(path: String): List[Long] =
    using(Source.fromFile(path))(_.getLines().map(_.trim).filterNot(_.isEmpty).map(_.toLong).toList)
}
