package com.kensai.aoc

import com.kensai.aoc.Day04._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day04Spec extends AnyFlatSpec with GivenWhenThen with Day04Fixtures {

  private val InputPath = "src/test/resources/Day04.input"
  private val SpecInputPath = "src/test/resources/Day04Spec.input"
  private val InvalidPasswordsInputPath = "src/test/resources/Day04-part2-invalid-passports.input"
  private val ValidPasswordsInputPath = "src/test/resources/Day04-part2-valid-passports.input"

  "parse Rows from spec" should "return 4 elements" in {
    // GIVEN: input from specs
    val input = readInputFile(SpecInputPath)

    // WHEN: parse
    val result = parse(input)

    // THEN: result is parsed
    result should have size 4
  }

  "parse Row1 from spec" should "return 1 valid elements" in {
    // GIVEN: input from specs
    val input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"

    // WHEN: parse
    val result = parse(input)

    // THEN: Row1 is ok
    val row1 = result.head
    row1(KeyEyeColor) should be("gry")
    row1(KeyPassportID) should be("860033327")
    row1(KeyExpirationYear) should be("2020")
    row1(KeyHairColor) should be("#fffffd")
    row1(KeyBirthYear) should be("1937")
    row1(KeyIssueYear) should be("2017")
    row1(KeyCountryID) should be("147")
    row1(KeyHeight) should be("183cm")
  }

  "parse Row2 from spec" should "return 1 valid elements" in {
    // GIVEN: input from specs
    val input = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929"

    // WHEN: parse
    val result = parse(input)

    // THEN: Row2 is ok
    val row2 = result.head
    println(row2)
    row2(KeyIssueYear) should be("2013")
    row2(KeyEyeColor) should be("amb")
    row2(KeyCountryID) should be("350")
    row2(KeyExpirationYear) should be("2023")
    row2(KeyPassportID) should be("028048884")
    row2(KeyHairColor) should be("#cfa07d")
    row2(KeyBirthYear) should be("1929")
  }

  "countValid for rows from spec" should "return 2" in {
    // GIVEN: input from specs
    val input = readInputFile(SpecInputPath)
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, FirstPartValidators)

    // THEN: count is 2
    result should be(2)
  }

  "countValid for rows" should "find solution for first part" in {
    // GIVEN: input from specs
    val input = readInputFile(InputPath)
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, FirstPartValidators)

    // THEN: count is 219
    result should be(219)
  }

  "countValid2 for rows from invalid passports" should "return 0" in {
    // GIVEN: input from specs
    val input = readInputFile(InvalidPasswordsInputPath)
    val passport = parse(input)

    // WHEN: countValid2
    val result = countValid(passport, SecondPartValidators)

    // THEN: count is 0
    result should be(0)
  }

  "countValid for rows from valid passports" should "return 4" in {
    // GIVEN: input from specs
    val input = readInputFile(ValidPasswordsInputPath)
    val passport = parse(input)

    // WHEN: countValid2
    val result = countValid(passport, SecondPartValidators)

    // THEN: count is 4
    result should be(4)
  }

  "countValid for rows" should "find solution for second part" in {
    // GIVEN: input from specs
    val input = readInputFile(InputPath)
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, SecondPartValidators)

    // THEN: count is 127
    result should be(127)
  }

  private def readInputFile(path: String): String =
    Source.fromFile(path)
      .getLines
      .toList
      .mkString("\n")

}
