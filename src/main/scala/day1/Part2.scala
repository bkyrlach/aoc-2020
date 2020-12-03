package day1

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

object Part2 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    inputs <- InputHelper.read("./inputs/day1/day1.txt")
    asInts = inputs.map(_.toInt)
    result <- findTriple(asInts) match {
      case Some(solution) =>
        val product = solution.product
        IO {
          println(s"$solution found, product is $product")
        }.as(ExitCode.Success)
      case None =>
        IO {
          println("No solution found in input")
        }.as(ExitCode.Error)
    }
  } yield {
    result
  }

  def slices(xs: List[Int], size: Int): List[List[Int]] = if(size <= 1) {
    xs.map(List(_))
  } else {
    xs match {
      case Nil  => Nil
      case h::t => slices(t, size - 1).map(h::_) ++ slices(t, size)
    }
  }

  def findTriple(xs: List[Int]): Option[List[Int]] =
    slices(xs, 3).find(_.sum == 2020)
}