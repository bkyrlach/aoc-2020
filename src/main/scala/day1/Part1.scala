package day1

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

object Part1 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    inputs <- InputHelper.read("./inputs/day1/day1.txt")
    asInts =  inputs.map(_.toInt)
    result <-  findPairs(asInts) match {
      case Some(pair) =>
        val product = pair._1 * pair._2
        IO { println(s"$pair found, product is $product") }.as(ExitCode.Success)
      case None =>
        IO { println("No solution found in input") }.as(ExitCode.Error)
    }
  } yield {
    result
  }

  def findPairs(xs: List[Int]): Option[(Int, Int)] = xs match {
    case Nil  => None
    case h::t => t.find(_ + h == 2020).map(n => h -> n) <+> findPairs(t)
  }
}