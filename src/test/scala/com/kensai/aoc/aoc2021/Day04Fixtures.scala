package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day04._

trait Day04Fixtures {

  val FirstPartValidators: List[PassportFieldValidator] =
    List(
      new PassportFieldExistValidator(KeyBirthYear),
      new PassportFieldExistValidator(KeyIssueYear),
      new PassportFieldExistValidator(KeyExpirationYear),
      new PassportFieldExistValidator(KeyHeight),
      new PassportFieldExistValidator(KeyHairColor),
      new PassportFieldExistValidator(KeyEyeColor),
      new PassportFieldExistValidator(KeyPassportID)
    )

  val SecondPartValidators: List[PassportFieldValidator] =
    List(
      PassportFieldBirthYearValidator,
      PassportFieldIssueYearValidator,
      PassportFieldExpirationYearValidator,
      PassportFieldHeightValidator,
      PassportFieldHairColorValidator,
      PassportFieldEyeColorValidator,
      PassportFieldIdValidator
    )

}
