package com.kensai.aoc.aoc2021

import Day04._
import com.kensai.aoc.lib.SimpleTestParam
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import org.scalatest.propspec.AnyPropSpec

class Day04PropSpec
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with Matchers
    with GivenWhenThen
    with Day04Fixtures {

  property("countValid should return expected value") {
    val inputs: TableFor1[SimpleTestParam[String, Int]] = Table(
      "Test parameters",
      SimpleTestParam(
        "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm",
        1
      ),
      SimpleTestParam(
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929",
        0
      )
    )

    forAll(inputs) { param =>
      Given(s"Input [${param.input}]")
      val passport = parse(param.input)

      When(s"countValid(${param.input})")
      val result = countValid(passport, FirstPartValidators)

      Then(s"Result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }

  property("PassportFieldValidator should return expected value") {
    case class PassportFieldValidatorTestParam(
        passport: Passport,
        validator: PassportFieldValidator,
        expectedResult: Boolean
    )

    val inputs: TableFor1[PassportFieldValidatorTestParam] = Table(
      "Test parameters",
      PassportFieldValidatorTestParam(
        Map(KeyBirthYear -> "2002"),
        PassportFieldBirthYearValidator,
        true
      ),
      PassportFieldValidatorTestParam(
        Map(KeyBirthYear -> "2003"),
        PassportFieldBirthYearValidator,
        false
      ),
      PassportFieldValidatorTestParam(
        Map(KeyHeight -> "60in"),
        PassportFieldHeightValidator,
        true
      ),
      PassportFieldValidatorTestParam(
        Map(KeyHeight -> "190cm"),
        PassportFieldHeightValidator,
        true
      ),
      PassportFieldValidatorTestParam(
        Map(KeyHeight -> "190in"),
        PassportFieldHeightValidator,
        false
      ),
      PassportFieldValidatorTestParam(
        Map(KeyHeight -> "190"),
        PassportFieldHeightValidator,
        false
      ),
      PassportFieldValidatorTestParam(
        Map(KeyHairColor -> "#123abc"),
        PassportFieldHairColorValidator,
        true
      ),
      PassportFieldValidatorTestParam(
        Map(KeyHairColor -> "#123abz"),
        PassportFieldHairColorValidator,
        false
      ),
      PassportFieldValidatorTestParam(
        Map(KeyHairColor -> "123abc"),
        PassportFieldHairColorValidator,
        false
      ),
      PassportFieldValidatorTestParam(
        Map(KeyEyeColor -> "brn"),
        PassportFieldEyeColorValidator,
        true
      ),
      PassportFieldValidatorTestParam(
        Map(KeyEyeColor -> "wat"),
        PassportFieldEyeColorValidator,
        false
      ),
      PassportFieldValidatorTestParam(
        Map(KeyPassportID -> "000000001"),
        PassportFieldIdValidator,
        true
      ),
      PassportFieldValidatorTestParam(
        Map(KeyPassportID -> "0123456789"),
        PassportFieldIdValidator,
        false
      )
    )

    forAll(inputs) { param =>
      Given(s"Input passport [${param.passport}]")
      And(s"Validator [${param.validator.getClass.getSimpleName}]")

      When(s"validate(${param.passport})")
      val result = param.validator(param.passport)

      Then(s"Result should be ${param.expectedResult}")
      result shouldBe param.expectedResult
    }
  }
}
