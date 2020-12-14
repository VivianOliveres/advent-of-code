package com.kensai.aoc

import com.kensai.aoc.Day14._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day14Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputFile("src/test/resources/Day14.input")
  private lazy val specInputs = readInputFile("src/test/resources/Day14Spec.input")

  "doComputePart1 for spec" should "return Map(7 -> 101, 8 -> 64)" in {
    val inputMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    Given(s"Input is Mask($inputMask)")
    val input = Input(
      mask = Mask(inputMask),
      instructions = List(
        Instruction(8, 11),
        Instruction(7, 101),
        Instruction(8, 0)
      )
    )

    When("doComputePart1")
    val result = doComputePart1(input)

    Then(s"Result is Map(7 -> 101, 8 -> 64)")
    result shouldBe Map(7 -> 101, 8 -> 64)
  }

  "parse for spec" should "return " in {
    Given(s"Input is spec")

    When("parse")
    val result = parse(specInputs)

    Then(s"Result is ")
    val expected = Input(
      mask = Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"),
      instructions = List(
        Instruction(8, 11),
        Instruction(7, 101),
        Instruction(8, 0)
      )
    )
    result shouldBe List(expected)
  }

  "parse for puzzle" should "return at least 2 expected" in {
    Given(s"Input is puzzle")

    When("parse")
    val result = parse(puzzleInputs)

    Then(s"Result is ")
    val expected1 = Input(
      mask = Mask("X00X0X0110010001010X0000000010001111"),
      instructions = List(
        Instruction(36604, 2508973),
        Instruction(14290, 80919),
        Instruction(6865, 15237),
        Instruction(45983, 393572789)
      )
    )
    result should contain (expected1)

    val expected2 = Input(
      mask = Mask("X101001X0XX00111000110X0010000110110"),
      instructions = List(
        Instruction(300, 253232),
        Instruction(5315, 56368),
        Instruction(64425, 18227),
        Instruction(20919, 4682858),
        Instruction(39194, 54595),
        Instruction(32891, 32798017)
      )
    )
    result should contain (expected2)
  }

  "compute for spec" should "return 165" in {
    Given(s"Input is spec")

    When("computePart1")
    val result = computePart1(specInputs)

    Then(s"Result is 165")
    result shouldBe 165
  }

  "computePart1 for puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When("compute")
    val result = computePart1(puzzleInputs)

    Then(s"Result is 5055782549997")
    result shouldBe 5055782549997L
  }


  "mask.computeAddresses for " should "return Set(26L, 27L, 58L, 59L)" in {
    val mask = Mask("000000000000000000000000000000X1001X")
    Given(s"Input is $mask")

    When("compute")
    val result = mask.computeAddresses(42L)

    Then(s"Result is Set(26L, 27L, 58L, 59L)")
    result shouldBe Set(26L, 27L, 58L, 59L)
  }

  "doComputePart2 for (42, 100)" should "return Map(26L -> 100L, 27L -> 100L, 58L -> 100L, 59L -> 100L)" in {
    val inputMask = "000000000000000000000000000000X1001X"
    Given(s"Input is Mask($inputMask)")
    val input = Input(
      mask = Mask(inputMask),
      instructions = List(
        Instruction(42, 100)
      )
    )

    When("doComputePart2")
    val result = doComputePart2(input)

    Then(s"Result is Map(26L -> 100L, 27L -> 100L, 58L -> 100L, 59L -> 100L)")
    result shouldBe Map(26L -> 100L, 27L -> 100L, 58L -> 100L, 59L -> 100L)
  }

  "doComputePart2 for (26, 1)" should "return Map(16L -> 1, 17L -> 1, 18L -> 1L, 19L -> 1L, 24L -> 1L, 25L -> 1L, 26L -> 1L, 27L -> 1L)" in {
    val inputMask = "00000000000000000000000000000000X0XX"
    Given(s"Input is Mask($inputMask)")
    val input = Input(
      mask = Mask(inputMask),
      instructions = List(
        Instruction(26, 1)
      )
    )

    When("doComputePart2")
    val result = doComputePart2(input)

    Then(s"Result is Map(16L -> 1, 17L -> 1, 18L -> 1L, 19L -> 1L, 24L -> 1L, 25L -> 1L, 26L -> 1L, 27L -> 1L)")
    result shouldBe Map(16L -> 1, 17L -> 1, 18L -> 1L, 19L -> 1L, 24L -> 1L, 25L -> 1L, 26L -> 1L, 27L -> 1L)
  }

  "computePart2 for spec" should "return 208" in {
    Given(s"Input is spec")
    val input1 = Input(
      mask = Mask("000000000000000000000000000000X1001X"),
      instructions = List(
        Instruction(42, 100)
      )
    )
    val input2 = Input(
      mask = Mask("00000000000000000000000000000000X0XX"),
      instructions = List(
        Instruction(26, 1)
      )
    )

    When("computePart2")
    val result = computePart2(List(input1, input2))

    Then(s"Result is 208")
    result shouldBe 208L
  }

  "computePart2 for puzzle" should "return solution" in {
    Given(s"Input is spec")

    When("computePart2")
    val result = computePart2(puzzleInputs)

    Then(s"Result is 4795970362286")
    result shouldBe 4795970362286L
  }

}
