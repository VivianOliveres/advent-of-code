package com.kensai.aoc.aoc2021

import Day14._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day14PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen {

  property("mask.computeLong should return valid value") {

    case class MaskComputeLongParam(
        maskInput: String,
        valueInput: Long,
        expectedResult: Long
    )

    val inputs = Table(
      "Test parameters",
      MaskComputeLongParam("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", 11L, 73L),
      MaskComputeLongParam("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", 101L, 101L),
      MaskComputeLongParam("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", 0L, 64L)
    )

    forAll(inputs) { param =>
      Given(s"Mask is ${param.maskInput}")
      val mask = Mask(param.maskInput)

      And(s"value is ${param.valueInput}")

      When(s"mask.computeLong(${param.valueInput})")
      val result = mask.computeLong(param.valueInput)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
