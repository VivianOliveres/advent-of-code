package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day03._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day03PropSpec extends AnyPropSpec with TableDrivenPropertyChecks with Matchers with GivenWhenThen {

  property("findShareItem should return result") {

    case class FindShareItemParam(input: String, expectedResult: Char)

    val inputs = Table(
      "Test parameters",
      FindShareItemParam("vJrwpWtwJgWrhcsFMMfFFhFp", 'p'),
      FindShareItemParam("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", 'L'),
      FindShareItemParam("PmmdzqPrVvPwwTWBwg", 'P'),
      FindShareItemParam("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", 'v'),
      FindShareItemParam("ttgJtRGJQctTZtZT", 't'),
      FindShareItemParam("CrZsJsPPZsGzwwsLwLmpwMDw", 's')
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")
      val input = param.input

      When(s"findShareItem(input)")
      val result = findSharedItem(input)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
