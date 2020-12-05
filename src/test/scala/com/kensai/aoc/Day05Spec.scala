package com.kensai.aoc

import com.kensai.aoc.Day05._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day05Spec extends AnyPropSpec with TableDrivenPropertyChecks with Matchers {

  case class TestInputs(input: String, expectedRow: Long, expectedColumn: Long, expectedSeatId: Long)

  val inputs = Table(
    "values",
    TestInputs("FBFBBFFRLR", 44L, 5L, 357L),
    TestInputs("BFFFBBFRRR", 70L, 7L, 567L),
    TestInputs("FFFBBBFRRR", 14L, 7L, 119L),
    TestInputs("BBFFBBFRLL", 102L, 4L, 820L)
  )

  property("computeRow and computeColumn and computeSeatId should be valid for spec inputs") {
    forAll (inputs) { values: TestInputs =>
      computeRow(values.input) shouldBe values.expectedRow
      computeColumn(values.input) shouldBe values.expectedColumn
      computeSeatId(values.input) shouldBe values.expectedSeatId
    }
  }

}
