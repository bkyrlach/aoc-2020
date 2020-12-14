package utils

import cats.effect.{IO, Resource}

import scala.io.Source

object InputHelper {

  def acquire(fileName: String): IO[Source] = IO {
    Source.fromFile(fileName)
  }

  def read(fileName: String): IO[List[String]] =
    Resource.fromAutoCloseable(acquire(fileName)).use(s => IO { s.getLines().toList })

//  def groupBy(inputs: List[String], p: String => Boolean): List[List[String]] = {
//    val (groups, last) = inputs.foldLeft(List.empty[List[String]] -> List.empty[String]) { (result, next) =>
//      if(p(next)) {
//        (result._2 :: result._1) -> Nil
//      } else {
//        result._1 -> (next::result._2)
//      }
//    }
//    groups :+ last
//  }
}