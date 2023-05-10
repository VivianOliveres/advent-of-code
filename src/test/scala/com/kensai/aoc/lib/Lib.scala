package com.kensai.aoc.lib

import scala.io.Source

import scala.language.reflectiveCalls

object Lib {

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try
      f(resource)
    finally
      resource.close()

  def readRawInputFile(path: String): String =
    using(Source.fromFile(path))(_.mkString)

  def readInputFile(path: String): String =
    using(Source.fromFile(path))(_.mkString)

  def readInputLines(path: String): List[String] =
    using(Source.fromFile(path))(_.getLines().toList)

  def readLongInputLines(path: String): List[Long] =
    using(Source.fromFile(path))(
      _.getLines().collect {
        case str if str.nonEmpty => str.trim
      }.map(_.toLong).toList
    )
}
