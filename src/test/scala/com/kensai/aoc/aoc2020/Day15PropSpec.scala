package com.kensai.aoc.aoc2020

import Day15._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen {

  property("lastSpokenAt should return result") {

    case class LastSpokenParam(
        input: String,
        turnNumber: Long,
        expectedResult: Long
    )

    val inputs = Table(
      "Test parameters",
      LastSpokenParam("0,3,6", 4L, 0L),
      LastSpokenParam("0,3,6", 5L, 3L),
      LastSpokenParam("0,3,6", 6L, 3L),
      LastSpokenParam("0,3,6", 7L, 1L),
      LastSpokenParam("0,3,6", 8L, 0L),
      LastSpokenParam("0,3,6", 9L, 4L),
      LastSpokenParam("0,3,6", 10L, 0L),
      LastSpokenParam("0,3,6", 2020L, 436L),
      LastSpokenParam("1,3,2", 2020L, 1L),
      LastSpokenParam("2,1,3", 2020L, 10L),
      LastSpokenParam("1,2,3", 2020L, 27L),
      LastSpokenParam("2,3,1", 2020L, 78L),
      LastSpokenParam("3,2,1", 2020L, 438L),
      LastSpokenParam("3,1,2", 2020L, 1836L),
      LastSpokenParam("0,3,6", 30000000L, 175594L),
      LastSpokenParam("1,3,2", 30000000L, 2578L),
      LastSpokenParam("2,1,3", 30000000L, 3544142L),
      LastSpokenParam("1,2,3", 30000000L, 261214L),
      LastSpokenParam("2,3,1", 30000000L, 6895259L),
      LastSpokenParam("3,2,1", 30000000L, 18L),
      LastSpokenParam("3,1,2", 30000000L, 362L)
    )

    forAll(inputs) { param =>
      Given(s"Mask is ${param.input}")
      val input = parse(param.input)

      When(s"lastSpokenAt(input, ${param.turnNumber})")
      val result = lastSpokenAt(input, param.turnNumber)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
