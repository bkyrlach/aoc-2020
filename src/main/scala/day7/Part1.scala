package day7

import cats._
import cats.implicits._
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp}
import utils.InputHelper

class ParserFor[I] {
  type ParserResult[A] = Either[String,A]
  type Parser[O] = StateT[ParserResult,List[I],O]

  def pure[A](a: A): Parser[A] = StateT.pure(a)
  def modify(update: List[I] => List[I]): Parser[Unit] =
    StateT.modify(update)
  def inspect[A](view: List[I] => A): Parser[A] =
    StateT.inspect[ParserResult,List[I],A](view)
  def error[A](err: String): Parser[A] =
    StateT.liftF(Left(err))

  def next: Parser[I] = for {
    maybeNext <- inspect[Option[I]](_.headOption)
    result    <- maybeNext match {
      case Some(n) => modify(_.tail) *> pure[I](n)
      case None    => error[I]("Unexpected end of input.")
    }
  } yield {
    result
  }

  def elemWhere(pred: I => Boolean, expected: String): Parser[I] = for {
    n      <- next
    result <- if(pred(n)) {
      pure[I](n)
    } else {
      error[I](s"Expected $expected, but got $n")
    }
  } yield {
    result
  }

  def elem(i: I): Parser[I] = elemWhere(_ == i, s"$i")

  def opt[O](p: Parser[O]): Parser[Option[O]] =
    p.map(Option.apply) <+> pure[Option[O]](Option.empty[O])

  def many[O](p: Parser[O]): Parser[List[O]] = for {
    maybeNext <- opt(p)
    result    <- maybeNext match {
      case Some(o) => many(p).map(o::_)
      case None    => pure(Nil)
    }
  } yield {
    result
  }

  def many1[O](p: Parser[O]): Parser[List[O]] = for {
    first <- p
    rest  <- many(p)
  } yield {
    first::rest
  }

  def sepBy1[O](p: Parser[O], s: Parser[_]): Parser[List[O]] = for {
    x <- many(p <* s)
    y <- p
  } yield {
    x ++ List(y)
  }

}

object Part1 extends IOApp {

  val charParser = new ParserFor[Char]
  import charParser._

  val digit: Parser[Char] = elemWhere(_.isDigit, "[0-9]")
  val number: Parser[Int] = many1(digit) map { _.mkString.toInt }
  val letter: Parser[Char] = elemWhere(_.isLetter, "[a-zA-Z]")
  val word: Parser[String] = many1(letter) map { _.mkString }

  case class Bag(description: String)

  val bag: Parser[Bag] = for {
    desc  <- word
    _     <- elem(' ')
    color <- word
    _     <- elem(' ')
    _     <- "bag".toList.traverse(elem) *> opt(elem('s'))
  } yield {
    Bag(s"$desc $color")
  }

  val edge: Parser[(Bag, Int)] = for {
    n <- number
    _ <- elem(' ')
    b <- bag
  } yield {
    b -> n
  }

  val edges: Parser[List[(Bag,Int)]] = sepBy1(edge, elem(',') *> elem(' '))

  val empty: Parser[List[(Bag,Int)]] = "no other bags".toList.traverse(elem).map(_ => List.empty[(Bag,Int)])

  case class Node[A](value: A, rels: List[(A,Int)])

  val node: Parser[Node[Bag]] = for {
    b  <- bag
    _  <- elem(' ')
    _  <- "contain".toList.traverse(elem)
    _  <- elem(' ')
    xs <- empty <+> edges
    _  <- elem('.')
  } yield {
    Node(b, xs)
  }

  def run(args: List[String]): IO[ExitCode] = for {
    inputs <- InputHelper.read("./inputs/day7/day7.txt")
    nodes  =  inputs.map { input => node.runA(input.toList) }.sequence
    _      <-  nodes match {
      case Right(list) =>
        val data = list.map(n => n.value -> n.rels).toMap
        println(data)
        val bags = canContain(Bag("shiny gold"), data)
        IO{ println(s"Number of bags: ${bags.size}") }
      case Left(err)   =>

        IO{ println(s"An error occurred: $err") }
    }
  } yield {
    ExitCode.Success
  }

  def canContain(b: Bag, env: Map[Bag, List[(Bag,Int)]]): Set[Bag] =
    env.keySet.filter(bag => contains(b, bag, env))

  def contains(inner: Bag, outer: Bag, env: Map[Bag, List[(Bag,Int)]]): Boolean = {
    val insideBags = env.getOrElse(outer, Nil).map(_._1)
    insideBags.contains(inner) || insideBags.exists(b => contains(inner, b, env))
  }

}