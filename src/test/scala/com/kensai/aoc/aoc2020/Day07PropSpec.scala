package com.kensai.aoc.aoc2020

import Day07._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day07PropSpec extends AnyPropSpec with TableDrivenPropertyChecks with Matchers with GivenWhenThen {

  property("parseColorContainedBy should be valid for spec inputs") {
    val inputs = Table(
      "Test parameters",
      SimpleTestParam(
        "light red bags contain 1 bright white bag, 2 muted yellow bags.",
        Map("bright white" -> "light red", "muted yellow" -> "light red")
      ),
      SimpleTestParam(
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
        Map("bright white" -> "dark orange", "muted yellow" -> "dark orange")
      ),
      SimpleTestParam(
        "bright white bags contain 1 shiny gold bag.",
        Map("shiny gold" -> "bright white")
      ),
      SimpleTestParam(
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
        Map("shiny gold" -> "muted yellow", "faded blue" -> "muted yellow")
      ),
      SimpleTestParam(
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
        Map("dark olive" -> "shiny gold", "vibrant plum" -> "shiny gold")
      ),
      SimpleTestParam(
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
        Map("faded blue" -> "dark olive", "dotted black" -> "dark olive")
      ),
      SimpleTestParam(
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
        Map("faded blue" -> "vibrant plum", "dotted black" -> "vibrant plum")
      ),
      SimpleTestParam("faded blue bags contain no other bags.", Map()),
      SimpleTestParam("dotted black bags contain no other bags.", Map())
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"parseColorContainedBy(${param.input})")
      val result = parseColorContainedBy(param.input)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("parseColorContaining should be valid for spec inputs") {
    val inputs = Table(
      "Test parameters",
      SimpleTestParam(
        "light red bags contain 1 bright white bag, 2 muted yellow bags.",
        Map("light red" -> Map("bright white" -> 1, "muted yellow" -> 2))
      ),
      SimpleTestParam(
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
        Map("dark orange" -> Map("bright white" -> 3, "muted yellow" -> 4))
      ),
      SimpleTestParam(
        "bright white bags contain 1 shiny gold bag.",
        Map("bright white" -> Map("shiny gold" -> 1))
      ),
      SimpleTestParam(
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
        Map("muted yellow" -> Map("shiny gold" -> 2, "faded blue" -> 9))
      ),
      SimpleTestParam(
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
        Map("shiny gold" -> Map("dark olive" -> 1, "vibrant plum" -> 2))
      ),
      SimpleTestParam(
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
        Map("dark olive" -> Map("faded blue" -> 3, "dotted black" -> 4))
      ),
      SimpleTestParam(
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
        Map("vibrant plum" -> Map("faded blue" -> 5, "dotted black" -> 6))
      ),
      SimpleTestParam(
        "faded blue bags contain no other bags.",
        Map("faded blue" -> Map())
      ),
      SimpleTestParam(
        "dotted black bags contain no other bags.",
        Map("dotted black" -> Map())
      )
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"parseColorContaining(${param.input})")
      val result = parseColorContaining(List(param.input))

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
