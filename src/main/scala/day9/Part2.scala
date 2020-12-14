package day9

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

import scala.annotation.tailrec

object Part2 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    inputs       <- InputHelper.read("./inputs/day9/day9.txt")
    inputNumbers =  inputs.map(BigInt(_))
    range        =  findRange(29221323, inputNumbers.toVector)
    _            <- range match {
      case Left(err) => IO { println(err) }
      case Right(r)  =>
        val answer = r.min + r.max
        IO { println(s"The sum of the min and max numbers of the range that adds up to 29221323 is $answer.")}
    }
  } yield {
    ExitCode.Success
  }

  def findRange(sum: BigInt, inputs: Vector[BigInt]): Either[String,Vector[BigInt]] = {
    def go(start: Int, end: Int): Either[String, Vector[BigInt]] = {
      val slice = inputs.slice(start, end)
      val sliceSum = slice.sum
      sliceSum.comparison(sum) match {
        case Comparison.GreaterThan =>
          val newStart = start+1
          if(newStart >= end) Left("Reached end of slice") else go(newStart, end)
        case Comparison.EqualTo => Right(slice)
        case Comparison.LessThan =>
          val newEnd = end+1
          if(newEnd >= inputs.size) Left("Reached end of inputs") else go(start, newEnd)
      }
    }
    go(0, 1)
  }
}