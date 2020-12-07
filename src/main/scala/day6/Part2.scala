package day6

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

object Part2 extends IOApp {

  val all = "abcdefghijklmnopqrstuvwxyz".toSet

  def run(args: List[String]): IO[ExitCode] = for {
    inputs     <- InputHelper.read("./inputs/day6/day6.txt")
    grouped    =  inputs.foldLeft(List.empty[List[String]] -> List.empty[String]) { (result, next) =>
      next match {
        case "" => (result._2 :: result._1) -> Nil
        case _  => result._1 -> (next::result._2)
      }
    }
    finalInput =  grouped._2::grouped._1
    processed  =  finalInput.map(answers => answers.map(_.toSet).foldLeft(all)(_.intersect(_)))
    answer     =  processed.map(_.size).sum
    _          <- IO { println(s"Sum of yes anwsers is $answer.") }
  } yield {
    ExitCode.Success
  }

}