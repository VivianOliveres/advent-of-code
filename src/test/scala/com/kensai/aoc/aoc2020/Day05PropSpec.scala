package com.kensai.aoc.aoc2020

import Day05._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day05PropSpec extends AnyPropSpec with TableDrivenPropertyChecks with Matchers with GivenWhenThen with Day05Fixtures {

  case class Day05TestParam(
      input: String,
      expectedRow: Long,
      expectedColumn: Long,
      expectedSeatId: Long)

  property(
    "computeRow and computeColumn and computeSeatId should be valid for spec inputs"
  ) {
    val inputs = Table(
      "Test parameters",
      Day05TestParam(Row1, 44L, 5L, 357L),
      Day05TestParam(Row2, 70L, 7L, 567L),
      Day05TestParam(Row3, 14L, 7L, 119L),
      Day05TestParam(Row4, 102L, 4L, 820L)
    )

    forAll(inputs) { param: Day05TestParam =>
      Given(s"Input is ${param.input}")

      Then(s"computeRow(${param.input}) should be ${param.expectedRow}")
      computeRow(param.input) shouldBe param.expectedRow

      Then(s"computeColumn(${param.input}) should be ${param.expectedColumn}")
      computeColumn(param.input) shouldBe param.expectedColumn

      Then(s"computeSeatId(${param.input}) should be ${param.expectedSeatId}")
      computeSeatId(param.input) shouldBe param.expectedSeatId
    }
  }

}
