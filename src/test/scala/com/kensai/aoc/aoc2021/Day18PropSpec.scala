package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day18._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day18PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen {

  property("parse should return valid Message") {
    val inputs = Table(
      "Test parameters",
      SimpleTestParam(
        "[1,2]",
        Parent(1, 2)
      ),
      SimpleTestParam(
        "[[1,2],3]",
        Parent(Parent(1, 2), 3)
      ),
      SimpleTestParam(
        "[9,[8,7]]",
        Parent(9, Parent(8, 7))
      ),
      SimpleTestParam(
        "[[1,9],[8,5]]",
        Parent(Parent(1, 9), Parent(8, 5))
      ),
      SimpleTestParam(
        "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]",
        Parent(
          Parent(
            Parent(Parent(1, 2), Parent(3, 4)),
            Parent(Parent(5, 6), Parent(7, 8))
          ),
          9
        )
      )
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"parse(${param.input})")
      val result = parse(param.input)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("explode should return valid result") {

    val inputs = Table(
      "Test parameters",
      SimpleTestParam("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
      SimpleTestParam("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
      SimpleTestParam("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
      SimpleTestParam(
        "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
        "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
      ),
      SimpleTestParam(
        "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
        "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
      ),
      SimpleTestParam(
        "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]",
        "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
      ),
      SimpleTestParam(
        "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]",
        "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
      )
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"explode(${param.input})")
      val message = parse(param.input)
      val result = explode(message)

      Then(s"result should be ${param.expectedResult}")
      val expected = parse(param.expectedResult)
      result shouldBe expected
    }
  }

}
