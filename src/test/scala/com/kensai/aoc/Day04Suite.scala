package com.kensai.aoc

import com.kensai.aoc.Day04._
import org.junit.runner.RunWith
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Day04Suite extends AnyFlatSpec {

  private val InputPath = "src/test/resources/Day04.input"
  private val SpecInputPath = "src/test/resources/Day04Spec.input"
  private val InvalidPasswordsInputPath = "src/test/resources/Day04-part2-invalid-passports.input"
  private val ValidPasswordsInputPath = "src/test/resources/Day04-part2-valid-passports.input"

  private val firstPartValidators: List[PassportFieldValidator] =
    List(
      new PassportFieldExistValidator(KeyBirthYear),
      new PassportFieldExistValidator(KeyIssueYear),
      new PassportFieldExistValidator(KeyExpirationYear),
      new PassportFieldExistValidator(KeyHeight),
      new PassportFieldExistValidator(KeyHairColor),
      new PassportFieldExistValidator(KeyEyeColor),
      new PassportFieldExistValidator(KeyPassportID))

  private val secondPartValidators: List[PassportFieldValidator] =
    List(
      PassportFieldBirthYearValidator,
      PassportFieldIssueYearValidator,
      PassportFieldExpirationYearValidator,
      PassportFieldHeightValidator,
      PassportFieldHairColorValidator,
      PassportFieldEyeColorValidator,
      PassportFieldIdValidator
    )


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

  "countValid Row1 from spec" should "return 1" in {
    // GIVEN: input from specs
    val input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, firstPartValidators)

    // THEN: count is 1
    result should be(1)
  }

  "countValid Row2 from spec" should "return 0" in {
    // GIVEN: input from specs
    val input = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929"
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, firstPartValidators)

    // THEN: count is 0
    result should be(0)
  }

  "countValid for rows from spec" should "return 2" in {
    // GIVEN: input from specs
    val input = readInputFile(SpecInputPath)
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, firstPartValidators)

    // THEN: count is 2
    result should be(2)
  }

  "countValid for rows" should "find solution for first part" in {
    // GIVEN: input from specs
    val input = readInputFile(InputPath)
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, firstPartValidators)

    // THEN: count is 219
    result should be(219)
  }

  "countValid2 for rows from invalid passports" should "return 0" in {
    // GIVEN: input from specs
    val input = readInputFile(InvalidPasswordsInputPath)
    val passport = parse(input)

    // WHEN: countValid2
    val result = countValid(passport, secondPartValidators)

    // THEN: count is 0
    result should be(0)
  }

  "countValid for rows from valid passports" should "return 4" in {
    // GIVEN: input from specs
    val input = readInputFile(ValidPasswordsInputPath)
    val passport = parse(input)

    // WHEN: countValid2
    val result = countValid(passport, secondPartValidators)

    // THEN: count is 4
    result should be(4)
  }

  "byr" should "be valid for 2002" in {
    // GIVEN: input
    val passport = Map(KeyBirthYear -> "2002")

    // WHEN: isValidBirthYear
    val result = PassportFieldBirthYearValidator(passport)

    // THEN: ok
    result should be(true)
  }

  "byr" should "be invalid for 2003" in {
    // GIVEN: input
    val passport = Map(KeyBirthYear -> "2003")

    // WHEN: isValidBirthYear
    val result = PassportFieldBirthYearValidator(passport)

    // THEN: ko
    result should be(false)
  }

  "hgt" should "be valid for 60in" in {
    // GIVEN: input
    val passport = Map(KeyHeight -> "60in")

    // WHEN: isValidHeight
    val result = PassportFieldHeightValidator(passport)

    // THEN: ok
    result should be(true)
  }

  "hgt" should "be valid for 190cm" in {
    // GIVEN: input
    val passport = Map(KeyHeight -> "190cm")

    // WHEN: isValidHeight
    val result = PassportFieldHeightValidator(passport)

    // THEN: ok
    result should be(true)
  }

  "hgt" should "be invalid for 190in" in {
    // GIVEN: input
    val passport = Map(KeyHeight -> "190in")

    // WHEN: isValidHeight
    val result = PassportFieldHeightValidator(passport)

    // THEN: false
    result should be(false)
  }

  "hgt" should "be invalid for 190" in {
    // GIVEN: input
    val passport = Map(KeyHeight -> "190")

    // WHEN: isValidHeight
    val result = PassportFieldHeightValidator(passport)

    // THEN: false
    result should be(false)
  }

  "hcl" should "be valid for #123abc" in {
    // GIVEN: input
    val passport = Map(KeyHairColor -> "#123abc")

    // WHEN: isValidHairColor
    val result = PassportFieldHairColorValidator(passport)

    // THEN: true
    result should be(true)
  }

  "hcl" should "be invalid for #123abz" in {
    // GIVEN: input
    val passport = Map(KeyHairColor -> "#123abz")

    // WHEN: isValidHairColor
    val result = PassportFieldHairColorValidator(passport)

    // THEN: false
    result should be(false)
  }

  "hcl" should "be invalid for 123abc" in {
    // GIVEN: input
    val passport = Map(KeyHairColor -> "123abc")

    // WHEN: isValidHairColor
    val result = PassportFieldHairColorValidator(passport)

    // THEN: false
    result should be(false)
  }

  "ecl" should "be valid for brn" in {
    // GIVEN: input
    val passport = Map(KeyEyeColor -> "brn")

    // WHEN: isValidEyeColor
    val result = PassportFieldEyeColorValidator(passport)

    // THEN: true
    result should be(true)
  }

  "ecl" should "be invalid for brn" in {
    // GIVEN: input
    val passport = Map(KeyEyeColor -> "wat")

    // WHEN: isValidEyeColor
    val result = PassportFieldEyeColorValidator(passport)

    // THEN: false
    result should be(false)
  }

  "pid" should "be valid for 000000001" in {
    // GIVEN: input
    val passport = Map(KeyPassportID -> "000000001")

    // WHEN: isValidPasswordId
    val result = PassportFieldIdValidator(passport)

    // THEN: true
    result should be(true)
  }

  "pid" should "be invalid for 0123456789" in {
    // GIVEN: input
    val passport = Map(KeyPassportID -> "0123456789")

    // WHEN: isValidPasswordId
    val result = PassportFieldIdValidator(passport)

    // THEN: false
    result should be(false)
  }

  "countValid for rows" should "find solution for second part" in {
    // GIVEN: input from specs
    val input = readInputFile(InputPath)
    val passport = parse(input)

    // WHEN: countValid
    val result = countValid(passport, secondPartValidators)

    // THEN: count is 127
    result should be(127)
  }

  private def readInputFile(path: String): String =
    Source.fromFile(path)
      .getLines
      .toList
      .mkString("\n")

}
