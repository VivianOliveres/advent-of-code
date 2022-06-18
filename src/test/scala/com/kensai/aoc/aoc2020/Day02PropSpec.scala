package com.kensai.aoc.aoc2020

import Day02._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day02PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen
    with Day02Fixtures {

  property("compute should return expected value") {
    val inputs: TableFor1[SimpleTestParam[String, Option[PasswordRow]]] = Table(
      "Test parameters",
      SimpleTestParam("1-3 a: abcde", Some(Row1)),
      SimpleTestParam("1-3 b: cdefg", Some(Row2)),
      SimpleTestParam("2-9 c: ccccccccc", Some(Row3)),
      SimpleTestParam("XYZccccccccccc", None),
      SimpleTestParam("", None)
    )

    forAll(inputs) { param =>
      Given(s"Input is [${param.input}]")

      When(s"parse(${param.input})")
      val result = parse(param.input)

      Then(s"Result is ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("isPasswordValidPart1 should return expected value") {
    val inputs: TableFor1[SimpleTestParam[PasswordRow, Boolean]] = Table(
      "Test parameters",
      SimpleTestParam(Row1, true),
      SimpleTestParam(Row2, false),
      SimpleTestParam(Row3, true)
    )

    forAll(inputs) { param =>
      Given(s"Input is [${param.input}]")

      When(s"isPasswordValidPart1(${param.input})")
      val result = isPasswordValidPart1(param.input)

      Then(s"Result is ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("isPasswordValidPart2 should return expected value") {
    val inputs: TableFor1[SimpleTestParam[PasswordRow, Boolean]] = Table(
      "Test parameters",
      SimpleTestParam(Row1, true),
      SimpleTestParam(Row2, false),
      SimpleTestParam(Row3, false)
    )

    forAll(inputs) { param =>
      Given(s"Input is [${param.input}]")

      When(s"isPasswordValidPart2(${param.input})")
      val result = isPasswordValidPart2(param.input)

      Then(s"Result is ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
