package day5

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

object Part1 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    inputs   <- InputHelper.read("./inputs/day5/day5.txt")
    seatIds  =  inputs.map(computeSeatId)
    lastSeat =  seatIds.max
    _        <- IO { println(s"The largest seat ID is $lastSeat") }
  } yield {
    ExitCode.Success
  }

  def computeSeatId(input: String): Int = {
    val row = computeRow(input.take(7))
    val col = computeColumn(input.drop(7))
    row * 8 + col
  }

  def computeRow(input: String): Int =
    (input.foldLeft(0 to 127) { (range, next) =>
      val first = range.head
      val last = range.last
      val delta = last - first
      val midPoint = first + Math.ceil(delta / 2.0).toInt
      next match {
        case 'F' => first to midPoint
        case 'B' => midPoint to last
      }
    }).head

  def computeColumn(input: String): Int =
    input.foldLeft(0 to 7) { (range, next) =>
      val first = range.head
      val last = range.last
      val delta = last - first
      val midPoint = first + Math.ceil(delta / 2.0).toInt
      next match {
        case 'L' => first to midPoint
        case 'R' => midPoint to last
      }
    }.head
}