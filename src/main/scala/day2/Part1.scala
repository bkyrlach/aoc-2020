package day2

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

import scala.util.matching.Regex

object Part1 extends IOApp {
  val password: Regex = """(\d+)-(\d+) (.): (.+)""".r

  case class PasswordRule(min: Int, max: Int, requirement: Char)

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
    password.groupBy(identity).get(rule.requirement).map { s =>
      val len = s.length
      if(len < rule.min) {
        Left(s"Password $password has only $len ${rule.requirement} but needs at least ${rule.min}")
      } else if (len > rule.max) {
        Left(s"Password $password has $len ${rule.requirement} but should only have at most ${rule.max}")
      } else {
        Right(password)
      }
    }.getOrElse(Left(s"Password $password doesn't contain required character ${rule.requirement}!"))
  }
}