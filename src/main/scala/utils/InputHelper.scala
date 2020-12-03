package utils

import cats.effect.{IO, Resource}

import scala.io.Source

object InputHelper {

  def acquire(fileName: String): IO[Source] = IO {
    Source.fromFile(fileName)
  }

  def read(fileName: String): IO[List[String]] =
    Resource.fromAutoCloseable(acquire(fileName)).use(s => IO { s.getLines().toList })
}