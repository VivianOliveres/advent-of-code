package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day16._
import com.kensai.aoc.lib.SimpleTestParam
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day16PropSpec extends AnyPropSpec with TableDrivenPropertyChecks with Matchers with GivenWhenThen {

  property("parse should return valid Message") {
    val inputs = Table(
      "Test parameters",
      SimpleTestParam(
        "110100101111111000101000",
        Message(6, LiteralPacket(2021))
      ),
      SimpleTestParam(
        "00111000000000000110111101000101001010010001001000000000",
        Message(
          1,
          OperatorPacket(
            6,
            Seq(
              Message(6, LiteralPacket(10)),
              Message(2, LiteralPacket(20))
            )
          )
        )
      ),
      SimpleTestParam(
        "11101110000000001101010000001100100000100011000001100000",
        Message(
          7,
          OperatorPacket(
            3,
            Seq(
              Message(2, LiteralPacket(1)),
              Message(4, LiteralPacket(2)),
              Message(1, LiteralPacket(3))
            )
          )
        )
      )
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"parse(${param.input})")
      val bits   = toBits(param.input)
      val result = parse(bits)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("computeSumVersionNumbers should return valid result") {

    val inputs = Table(
      "Test parameters",
      SimpleTestParam("8A004A801A8002F478", 16),
      SimpleTestParam("620080001611562C8802118E34", 12),
      SimpleTestParam("C0015000016115A2E0802F182340", 23),
      SimpleTestParam("A0016C880162017C3686B18A3D4780", 31)
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"computeSumVersionNumbers(${param.input})")
      val bits    = hexToBin(param.input)
      val message = parse(bits)
      val result  = computeSumVersionNumbers(message)

      Then(s"result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("eval should return valid result") {

    val inputs = Table(
      "Test parameters",
      SimpleTestParam("04005AC33890", 54L),
      SimpleTestParam("880086C3E88112", 7L),
      SimpleTestParam("CE00C43D881120", 9L),
      SimpleTestParam("D8005AC2A8F0", 1L),
      SimpleTestParam("F600BC2D8F", 0L),
      SimpleTestParam("9C005AC2F8F0", 0L),
      SimpleTestParam("9C0141080250320F1802104A08", 1L)
    )

    forAll(inputs) { param =>
      Given(s"Input is ${param.input}")

      When(s"eval(${param.input})")
      val bits    = hexToBin(param.input)
      val message = parse(bits)
      val result  = message.eval

      Then(s"Result is ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }
}
