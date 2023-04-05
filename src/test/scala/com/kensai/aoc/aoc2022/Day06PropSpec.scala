package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day06._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day06PropSpec extends AnyPropSpec with TableDrivenPropertyChecks with Matchers with GivenWhenThen {

  property("startPacketMarker should return result") {

    case class StartPacketMarkerParam(input: String, expectedResult: Int)

    val inputs = Table(
      "Test parameters",
      StartPacketMarkerParam("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
      StartPacketMarkerParam("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
      StartPacketMarkerParam("nppdvjthqldpwncqszvftbrmjlhg", 6),
      StartPacketMarkerParam("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
      StartPacketMarkerParam("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")
      val input = param.input

      When(s"startPacketMarker(input)")
      val result = startPacketMarker(input)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("startMessageMarker should return result") {

    case class StartMessageMarkerParam(input: String, expectedResult: Int)

    val inputs = Table(
      "Test parameters",
      StartMessageMarkerParam("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
      StartMessageMarkerParam("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
      StartMessageMarkerParam("nppdvjthqldpwncqszvftbrmjlhg", 23),
      StartMessageMarkerParam("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
      StartMessageMarkerParam("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")
      val input = param.input

      When(s"startMessageMarker(input)")
      val result = startMessageMarker(input)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

}
