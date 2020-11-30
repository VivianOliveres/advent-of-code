package com.kensai.aoc

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day01Suite extends AnyFunSuite {

  test("someMethod is always true") {
    def library = new Day01()
    assert(library.someMethod)
  }

}
