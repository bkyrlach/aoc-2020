package day4

import cats._
import cats.data.{ValidatedNec}
import cats.data.Validated.Valid
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import utils.InputHelper

object Part2 extends IOApp {

  sealed trait Height
  case class Inches(n: Int) extends Height
  case class Centimeters(n: Int) extends Height

  sealed trait Color
  case object Amber extends Color
  case object Blue extends Color
  case object Brown extends Color
  case object Gray extends Color
  case object Green extends Color
  case object Hazel extends Color
  case object Other extends Color

  case class Passport(birthYear: Int, issueYear: Int, expirationYear: Int, height: Height, hairColor: String, eyeColor: Color, passportId: String, countryId: Option[String])

  def run(args: List[String]): IO[ExitCode] = for {
    inputs         <- InputHelper.read("./inputs/day4/day4.txt")
    fixedInputs    =  inputs.foldRight((List.empty[String], "")) { (next, result) =>
      if(next == "") {
        (result._2 :: result._1, "")
      } else {
        (result._1, if(result._2 == "") next else result._2 + " " + next)
      }
    }
    parsedInputs   =  (fixedInputs._2 :: fixedInputs._1).map(parsePassport)
    _              <- parsedInputs.traverse { passport => IO { println(passport) }}
    validPassports =  parsedInputs.collect {
      case Valid(passport) => passport
    }
    _              <- IO { println(s"There were ${validPassports.size} valid passports.") }
  } yield {
    ExitCode.Success
  }

  type ValidationResult[A] = ValidatedNec[String,A]

  def parsePassport(s: String): ValidationResult[Passport] = {
    val dataMap = s.split(' ').map { part =>
      val kv = part.split(':')
      (kv(0), kv(1))
    }.toMap
    (validateBirthYear(dataMap.get("byr")),
     validateIssueYear(dataMap.get("iyr")),
     validateExpirationYear(dataMap.get("eyr")),
     validateHeight(dataMap.get("hgt")),
     validateHairColor(dataMap.get("hcl")),
     validateEyeColor(dataMap.get("ecl")),
     validatePassportId(dataMap.get("pid")),
     dataMap.get("cid").validNec
    ).mapN(Passport)
  }

  def validateIntRange(n: Int, min: Int, max: Int, name: String): ValidationResult[Int] =
    if(n < min) {
      s"$name ($n) is below expected min $min".invalidNec
    } else if (n > max) {
      s"$name ($n) is above expected max $max".invalidNec
    } else {
      n.validNec
    }

  def validateBirthYear(input: Option[String]): ValidationResult[Int] =
    input.flatMap(_.toIntOption) match {
      case Some(year) => validateIntRange(year, 1920, 2002, "Birth Year")
      case None => "Birth Year is missing and is required.".invalidNec
    }

  def validateIssueYear(input: Option[String]): ValidationResult[Int] =
    input.flatMap(_.toIntOption) match {
      case Some(year) => validateIntRange(year, 2010, 2020, "Issue Year")
      case None => "Issue Year is missing and is required.".invalidNec
    }

  def validateExpirationYear(input: Option[String]): ValidationResult[Int] =
    input.flatMap(_.toIntOption) match {
      case Some(year) => validateIntRange(year, 2020, 2030, "Expiration Year")
      case None => "Expiration Year is missing and is required.".invalidNec
    }

  val heightCm = """(\d+)cm""".r
  val heightIn = """(\d+)in""".r

  def validateHeight(input: Option[String]): ValidationResult[Height] =
    input match {
      case Some(heightStr) => heightStr match {
        case heightCm(cm) => validateIntRange(cm.toInt, 150, 193, "Height(cm)").map(Centimeters)
        case heightIn(in) => validateIntRange(in.toInt, 59, 76, "Height(in)").map(Inches)
        case _ => s"$heightStr isn't recognizable as a height.".invalidNec
      }
      case None => "Height is missing and is required.".invalidNec
    }

  val colorStr = """#([a-f0-9]{6})""".r

  def validateHairColor(input: Option[String]): ValidationResult[String] =
    input match {
      case Some(str) => str match {
        case colorStr(data) => s"#$data".validNec
        case _              => s"$str doesn't match expected format for Hair Color.".invalidNec
      }
      case None => "Hair Color is missing and is required.".invalidNec
    }

  def validateEyeColor(input: Option[String]): ValidationResult[Color] =
    input match {
      case Some(str) => str match {
        case "amb" => Amber.validNec
        case "blu" => Blue.validNec
        case "brn" => Brown.validNec
        case "gry" => Gray.validNec
        case "grn" => Green.validNec
        case "hzl" => Hazel.validNec
        case "oth" => Other.validNec
        case unk   => s"$unk is not a recognized eye color.".invalidNec
      }
      case None => "Eye Color is missing and is required.".invalidNec
    }

  val passportId = """(\d{9})""".r

  def validatePassportId(input: Option[String]): ValidationResult[String] =
    input match {
      case Some(str) => str match {
        case passportId(id) => id.validNec
        case _              => s"$str doesn't match the expected passport ID format.".invalidNec
      }
      case None => "Passport ID is missing and is required.".invalidNec
    }
}