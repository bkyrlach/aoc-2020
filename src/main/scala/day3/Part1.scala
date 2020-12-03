package day3

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

import scala.util.matching.Regex

object Part1 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    inputs           <- InputHelper.read("./inputs/day3/day3.txt")
    treesEncountered =  traverseSlope(3,1, 1, inputs)
    _                <- IO {
      println(s"Traversing the slope (3, 1), you'll encounter $treesEncountered.")
    }
  } yield {
    ExitCode.Success
  }

  def traverseSlope(right: Int, down: Int, step: Int, terrain: List[String]): Int = {
    val stepDown = down * step
    val stepRight = right * step
    if(stepDown >= terrain.size) {
      0
    } else {
      val line = terrain(stepDown)
      val toCheck = stepRight % line.length
      (if(line.charAt(toCheck) == '#') {
        1
      } else {
        0
      }) + traverseSlope(right, down, step + 1, terrain)
    }
  }
}