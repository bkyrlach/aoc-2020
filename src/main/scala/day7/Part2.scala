package day7

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import day7.Part1.{Bag, node}
import utils.InputHelper

object Part2 extends IOApp {

  def run(args: List[String]): IO[ExitCode] = for {
    inputs       <- InputHelper.read("./inputs/day7/day7.txt")
    parseResult  =  inputs.map { input => node.runA(input.toList) }.sequence
    nodes        <- parseResult match {
      case Right(list) => IO.pure(list)
      case Left(err)   => IO.raiseError(new RuntimeException(err))
    }
    edgeWeightSum =  edgeWeightsFrom(Bag("shiny gold"), nodes.map(n => n.value -> n.rels).toMap)
    _             <- IO { println(s"$edgeWeightSum")}
  } yield {
    ExitCode.Success
  }

  def edgeWeightsFrom(b: Bag, env: Map[Bag,List[(Bag,Int)]]): Int = 
    env.getOrElse(b, Nil).map {
      case (o,w) => (1 + edgeWeightsFrom(o, env)) * w
    }.sum
}