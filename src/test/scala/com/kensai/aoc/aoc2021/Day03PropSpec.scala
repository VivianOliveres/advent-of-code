package com.kensai.aoc.aoc2021

import Day03._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day03PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen
    with Day03Fixtures {

  case class Day03TestParam(
      row: String,
      index: Int,
      expectedResult: TobogganRow
  )

  property("compute should return expected value") {
    val inputs: TableFor1[Day03TestParam] = Table(
      "Test parameters",
      Day03TestParam(Row1, 0, ExpectedRow1),
      Day03TestParam(Row2, 1, ExpectedRow2)
    )

    forAll(inputs) { param =>
      Given(s"Input is [${param.row}]")

      When(s"parse(${param.row}, ${param.index})")
      val result = parse(param.row, param.index)

      Then(s"Result is ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
