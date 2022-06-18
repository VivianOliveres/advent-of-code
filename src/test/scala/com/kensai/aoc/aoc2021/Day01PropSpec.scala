package com.kensai.aoc.aoc2021

import Day01._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day01PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen {

  val ExpectedSum = 2020L

  property("compute should return expected value") {
    val inputs: TableFor1[SimpleTestParam[List[Long], Option[Long]]] = Table(
      "Test parameters",
      SimpleTestParam(List(1721L, 979L, 366L, 299L, 675L, 1456L), Some(514579)),
      SimpleTestParam(List(3L, 5L), None),
      SimpleTestParam(List(), None)
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"compute($ExpectedSum, ${param.input})")
      val result = compute(ExpectedSum, param.input)

      Then(s"Result is ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("compute3 should return expected value") {
    val inputs: TableFor1[SimpleTestParam[List[Long], Option[Long]]] = Table(
      "Test parameters",
      SimpleTestParam(
        List(1721L, 979L, 366L, 299L, 675L, 1456L),
        Some(241861950L)
      ),
      SimpleTestParam(List(3L, 5L, 18L), None),
      SimpleTestParam(List(), None)
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"compute3($ExpectedSum, ${param.input})")
      val result = compute3(ExpectedSum, param.input)

      Then(s"Result is ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
