package day3

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

object Part2 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    inputs <- InputHelper.read("./inputs/day3/day3.txt")
    run1   =  traverseSlope(1,1, 1, inputs)
    run2   =  traverseSlope(3,1, 1, inputs)
    run3   =  traverseSlope(5,1, 1, inputs)
    run4   =  traverseSlope(7,1, 1, inputs)
    run5   =  traverseSlope(1,2, 1, inputs)
    _      <- IO {
      val treeProduct = run1 * run2 * run3 * run4 * run5
      println(treeProduct)
    }
  } yield {
    ExitCode.Success
  }

  def traverseSlope(right: Int, down: Int, step: Int, terrain: List[String]): Long = {
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