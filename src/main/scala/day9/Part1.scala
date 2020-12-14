package day9

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

import scala.annotation.tailrec

object Part1 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    inputs       <- InputHelper.read("./inputs/day9/day9.txt")
    inputNumbers =  inputs.map(BigInt(_))
    result       =  checkSequence(inputNumbers)
    _            <- result match {
      case Left(invalid) => IO { println(s"$invalid doesn't follow the rules.")}
      case Right(msg)      => IO { println(msg)}
    }
  } yield {
    ExitCode.Success
  }

  def distinctPairs(xs: List[BigInt]): List[(BigInt, BigInt)] = {
    val unique = xs.distinct
    for {
      x <- unique
      y <- unique.filterNot(_ == x)
    } yield {
      x -> y
    }
  }

  @tailrec
  def checkSequence(inputs: List[BigInt]): Either[BigInt,String] = {
    inputs.drop(25).headOption match {
      case None    => Right("All inputs appeared to follow the rules.")
      case Some(v) =>
        val checkSeq = inputs.take(25)
        val pairs = distinctPairs(checkSeq)
        val sums = pairs.map(p => p._1 + p._2)
        if(sums.contains(v)) {
          checkSequence(inputs.drop(1))
        } else {
          Left(v)
        }
    }
  }
}