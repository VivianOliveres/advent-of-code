package com.kensai.aoc.aoc2021

import Day08._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day08PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen {

  property("parseColorContainedBy should be valid for spec inputs") {
    val inputs = Table(
      "Test parameters",
      SimpleTestParam("nop +0", Nop(0, 0)),
      SimpleTestParam("acc +1", Acc(0, 1)),
      SimpleTestParam("jmp -3", Jump(0, -3))
    )

    forAll(inputs) { param =>
      val index = 0
      Given(s"Input is ${param.input} and index is $index")

      When(s"parseColorContainedBy(${param.input})")
      val result = parseRow(param.input, index)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }
}
