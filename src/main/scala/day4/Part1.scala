package day4

import cats._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import utils.InputHelper

object Part1 extends IOApp {

  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

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
    validPassports =  parsedInputs.filter(validatePassport)
    _              <- IO { println(s"There were ${validPassports.size} valid passports.") }
  } yield {
    ExitCode.Success
  }

  def parsePassport(s: String): Map[String,String] = s.split(' ').map { part =>
    val kv = part.split(':')
    (kv(0), kv(1))
  }.toMap

  def validatePassport(passport: Map[String,String]): Boolean =
    passport.keySet.intersect(required) == required
}