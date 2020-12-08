package com.kensai.aoc

import scala.util.matching.Regex

object Day04 {

  val KeyBirthYear = "byr"
  val KeyIssueYear = "iyr"
  val KeyExpirationYear = "eyr"
  val KeyHeight = "hgt"
  val KeyHairColor = "hcl"
  val KeyEyeColor = "ecl"
  val KeyPassportID = "pid"
  val KeyCountryID = "cid"

  /**
   * Type for passport.
   */
  type Passport = Map[String, String]

  /**
   * Base class to validate if a Passport is valid
   */
  sealed trait PassportFieldValidator extends Function[Passport, Boolean] {
    def name(): String
  }

  /**
   * Generic validator that check if the given `Passport` has the expected key `name`.
   */
  class PassportFieldExistValidator(val name: String) extends PassportFieldValidator {
    override def apply(passport: Passport): Boolean = passport.contains(name)
  }

  /**
   * Validator that check the birth year field.
   */
  case object PassportFieldBirthYearValidator extends PassportFieldValidator {
    override def name(): String = KeyBirthYear

    override def apply(passport: Passport): Boolean =
      passport.get(name())
        .exists(year => year.length == 4 && year.toInt >= 1920 && year.toInt <= 2002)
  }

  /**
   * Validator that check the issue year field.
   */
  case object PassportFieldIssueYearValidator extends PassportFieldValidator {
    override def name(): String = KeyIssueYear

    override def apply(passport: Passport): Boolean =
      passport.get(name())
        .exists(year => year.length == 4 && year.toInt >= 2010 && year.toInt <= 2020)
  }

  /**
   * Validator that check the expiration year field.
   */
  case object PassportFieldExpirationYearValidator extends PassportFieldValidator {
    override def name(): String = KeyExpirationYear

    override def apply(passport: Passport): Boolean =
      passport.get(name())
        .exists(year => year.length == 4 && year.toInt >= 2020 && year.toInt <= 2030)
  }

  /**
   * Validator that check the height year field.
   */
  case object PassportFieldHeightValidator extends PassportFieldValidator {
    override def name(): String = KeyHeight

    private val cmRegex = """(\d+)cm""".r
    private val inRegex = """(\d+)in""".r
    private val numberRegex = """(\d+)""".r

    override def apply(passport: Passport): Boolean = {
      passport.get(name())
        .exists(hgt => hgt match {
          case cmRegex(h) => !(h.toInt < 150 || h.toInt > 193)
          case inRegex(h) => !(h.toInt < 59 || h.toInt > 76)
          case numberRegex(_) => false
        })
    }
  }

  /**
   * Validator that check the hear color field.
   */
  case object PassportFieldHairColorValidator extends PassportFieldValidator {
    override def name(): String = KeyHairColor

    val HairColorRegex: Regex = "#[0-9a-f]{6}".r

    override def apply(passport: Passport): Boolean =
      passport.get(name())
        .exists(color => HairColorRegex.pattern.matcher(color).find() && color.length == 7)
  }

  /**
   * Validator that check the eye color field.
   */
  case object PassportFieldEyeColorValidator extends PassportFieldValidator {
    override def name(): String = KeyEyeColor

    override def apply(passport: Passport): Boolean =
      passport.get(name())
        .exists(color => color == "amb" || color == "blu" || color == "brn" || color == "gry" || color == "grn" || color == "hzl" || color == "oth")
  }

  /**
   * Validator that check the PassportId color field.
   */
  case object PassportFieldIdValidator extends PassportFieldValidator {
    override def name(): String = KeyPassportID

    val PIdRegex: Regex = "[0-9]{9}".r

    override def apply(passport: Passport): Boolean =
      passport.get(name())
        .exists(pid => PIdRegex.pattern.matcher(pid).find() && pid.length == 9)
  }

  /**
   * Parse a `String` into a list of `Passport`
   */
  def parse(input: String): List[Passport] = {
    try {
      input.split("\n\n")
        .toList
        .filterNot(_.isEmpty)
        .map(toPassport)

    } catch {
      case _: RuntimeException => {
        System.err.println(s"Day04 - parse - Invalid format for input[$input]")
        List()
      }
    }
  }

  private def toPassport(row: String): Passport =
    row.split("[ \n]")
      .toList
      .map(_.split(":").toList.map(_.trim).filterNot(_.isEmpty))
      .filter(_.size == 2)
      .map(fields => fields.head -> fields(1))
      .toMap

  def countValid(passports: List[Passport], validators: List[PassportFieldValidator]): Int =
    passports.count(passport => validators.forall(_.apply(passport)))

}
