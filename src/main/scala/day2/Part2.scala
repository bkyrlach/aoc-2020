package day2

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

import scala.util.matching.Regex

object Part2 extends IOApp {
  val password: Regex = """(\d+)-(\d+) (.): (.+)""".r

  case class PasswordRule(first: Int, second: Int, requirement: Char)

  def run(args: List[String]): IO[ExitCode] = for {
    inputs    <- InputHelper.read("./inputs/day2/day2.txt")
    processed =  inputs map {
      case password(minOccurs, maxOccurs, char, password) =>
        val rule = PasswordRule(minOccurs.toInt, maxOccurs.toInt, char.head)
        (rule, password)
    }
    results   =  processed.map(validate)
    _         <- IO {
      val validCount = results.collect {
        case Right(pwd) => pwd
      }.size
      println(s"There were $validCount passwords that are correct according to the given rules.")
    }
  } yield {
    ExitCode.Success
  }

  def validate(input: (PasswordRule, String)): Either[String,String] = {
    val rule = input._1
    val password = input._2
    val sat1 = password.charAt(rule.first - 1) == rule.requirement
    val sat2 = password.charAt(rule.second - 1) == rule.requirement
    val result = sat1 ^ sat2
    if(result) {
      Right(password)
    } else {
      if(sat1 && sat2) {
        Left(s"Password $password had the required character at ${rule.first} and ${rule.second}, which is invalid.")
      } else {
        Left(s"Password $password didn't have the required character at either ${rule.first} or ${rule.second}, which is invalid.")
      }
    }
  }
}