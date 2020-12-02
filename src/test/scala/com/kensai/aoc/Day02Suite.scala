package com.kensai.aoc

import com.kensai.aoc.Day02._
import org.scalatest._
import matchers.should.Matchers._
import flatspec._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day02Suite extends AnyFlatSpec {

  private val Row1: PasswordRow = PasswordRow(1, 3, 'a', "abcde")
  private val Row2: PasswordRow = PasswordRow(1, 3, 'b', "cdefg")
  private val Row3: PasswordRow = PasswordRow(2, 9, 'c', "ccccccccc")

  "parse Row1" should "return a valid PasswordRow" in {
    // GIVEN: input from specs
    val input = "1-3 a: abcde"

    // WHEN: parse
    val result = parse(input)

    // THEN: result is parsed
    result should be (Some(Row1))
  }

  "parse Row2" should "return a valid PasswordRow" in {
    // GIVEN: input from specs
    val input = "1-3 b: cdefg"

    // WHEN: parse
    val result = parse(input)

    // THEN: result is parsed
    result should be (Some(Row2))
  }

  "parse Row3" should "return a valid PasswordRow" in {
    // GIVEN: input from specs
    val input = "2-9 c: ccccccccc"

    // WHEN: parse
    val result = parse(input)

    // THEN: result is parsed
    result should be (Some(Row3))
  }

  "parse empty line" should "return None" in {
    // GIVEN: empty line
    val input = ""

    // WHEN: parse
    val result = parse(input)

    // THEN: result is parsed
    result should be (None)
  }

  "parse invalid line" should "return None" in {
    // GIVEN: invalid line
    val input = "XYXccccccccc"

    // WHEN: parse
    val result = parse(input)

    // THEN: result is parsed
    result should be (None)
  }

  "isPasswordValidPart1 for Row1" should "return True" in {
    // GIVEN: specs
    val inputs = Row1

    // WHEN: isPasswordValidPart1
    val result = isPasswordValidPart1(inputs)

    // THEN: result is True
    result should be (true)
  }

  "isPasswordValidPart1 for Row2" should "return False" in {
    // GIVEN: specs
    val inputs = Row2

    // WHEN: isPasswordValidPart1
    val result = isPasswordValidPart1(inputs)

    // THEN: result is False
    result should be (false)
  }

  "isPasswordValidPart1 for Row3" should "return True" in {
    // GIVEN: specs
    val inputs = Row3

    // WHEN: isPasswordValidPart1
    val result = isPasswordValidPart1(inputs)

    // THEN: result is True
    result should be (true)
  }

  "validPasswordCount for rows from specs" should "return 2 for Part1" in {
    // GIVEN: specs
    val inputs = List(Row1, Row2, Row3)

    // WHEN: validPasswordCount
    val result = validPasswordCount(inputs)(isPasswordValidPart1)

    // THEN: result is 2
    result should be (2)
  }

  "validPasswordCount" should "return solution for Part1" in {
    // GIVEN: inputs
    val inputs = readInputFile("src/test/resources/Day02.input")

    // WHEN: validPasswordCount
    val result = validPasswordCount(inputs)(isPasswordValidPart1)

    // THEN: print solution
    println(s"Solution is $result")
    result should (be > 0)
  }

  private def readInputFile(path: String): List[PasswordRow] =
    Source.fromFile(path)
      .getLines
      .map(_.trim)
      .filterNot(_.isEmpty)
      .flatMap(parse)
      .toList


  "isPasswordValidPart2 for Row1" should "return True" in {
    // GIVEN: specs
    val inputs = Row1

    // WHEN: isPasswordValidPart1
    val result = isPasswordValidPart2(inputs)

    // THEN: result is True
    result should be (true)
  }

  "isPasswordValidPart2 for Row2" should "return False" in {
    // GIVEN: specs
    val inputs = Row2

    // WHEN: isPasswordValidPart1
    val result = isPasswordValidPart2(inputs)

    // THEN: result is False
    result should be (false)
  }

  "isPasswordValidPart2 for Row3" should "return False" in {
    // GIVEN: specs
    val inputs = Row3

    // WHEN: isPasswordValidPart1
    val result = isPasswordValidPart2(inputs)

    // THEN: result is False
    result should be (false)
  }

  "validPasswordCount for rows from specs" should "return 1 for Part2" in {
    // GIVEN: specs
    val inputs = List(Row1, Row2, Row3)

    // WHEN: validPasswordCount
    val result = validPasswordCount(inputs)(isPasswordValidPart2)

    // THEN: result is 1
    result should be (1)
  }

  "validPasswordCount" should "return solution for Part2" in {
    // GIVEN: inputs
    val inputs = readInputFile("src/test/resources/Day02.input")

    // WHEN: validPasswordCount
    val result = validPasswordCount(inputs)(isPasswordValidPart2)

    // THEN: print solution
    println(s"Solution is $result")
    result should (be > 0)
  }

}
