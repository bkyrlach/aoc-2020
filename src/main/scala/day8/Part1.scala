package day8

import cats._
import cats.data.State
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import day8.Computer.{Accumulate, Instruction, Jump, NoOp}
import utils.InputHelper

object Part1 extends IOApp {

  import day7.ParserFor

  val p = new ParserFor[Char]
  import p._

  def str(s: String): Parser[String] =
    s.toList.traverse(elem).as(s)

  val digit: Parser[Char] = elemWhere(_.isDigit, "[0-9]")
  val plus: Parser[Char] = elem('+')
  val minus: Parser[Char] = elem('-')

  val number: Parser[Int] = for {
    sign   <- plus <+> minus
    digits <- many1(digit).map(_.mkString)
  } yield {
    sign match {
      case '-' => s"-$digits".toInt
      case '+' => digits.toInt
    }
  }

  val acc: Parser[String] = str("acc")
  val jmp: Parser[String] = str("jmp")
  val nop: Parser[String] = str("nop")

  val instruction: Parser[Instruction] = for {
    inStr <- acc <+> jmp <+> nop
    _     <- elem(' ')
    value <- number
  } yield {
    inStr match {
      case "acc" => Accumulate(value)
      case "jmp" => Jump(value)
      case "nop" => NoOp
    }
  }

  def run(args: List[String]): IO[ExitCode] = for {
    inputs  <- InputHelper.read("./inputs/day8/day8.txt")
    program =  inputs.map(input => instruction.runA(input.toList)).sequence match {
      case Right(instructions) => instructions
      case Left(_)             => Nil
    }
    result  =  Computer.runProgram(program.toVector)
    _       <- IO { println(s"Accumulator state before loop was detected is $result.") }
  } yield {
    ExitCode.Success
  }

}

object Computer {
  sealed trait Instruction
  case class Accumulate(value: Int) extends Instruction
  case class Jump(value: Int) extends Instruction
  case object NoOp extends Instruction

  case class MachineState(instructionPointer: Int, accumulator: Int, program: Vector[Instruction], visited: Set[Int])

  type Program[A] = State[MachineState,A]

  val get: Program[MachineState] = State.get[MachineState]
  def pure[A](a: A): Program[A] = State.pure[MachineState,A](a)
  def inspect[A](view: MachineState => A): Program[A] = State.inspect[MachineState,A](view)
  def mod(update: MachineState => MachineState): Program[Unit] = State.modify[MachineState](update)

  def runProgram(program: Vector[Instruction]): Int = {
    val initState = MachineState(
      instructionPointer = 0,
      accumulator = 0,
      program = program,
      visited = Set.empty[Int]
    )
    exec.runS(initState).value.accumulator
  }

  val exec: Program[Unit] = for {
    isLoop <- inspect(ms => ms.visited.contains(ms.instructionPointer))
    _      <- if(isLoop) pure(()) else (inspect(ms => ms.program(ms.instructionPointer)) >>= {
      case NoOp              => mod(ms =>
        ms.copy(
          visited = ms.visited + ms.instructionPointer,
          instructionPointer = ms.instructionPointer + 1
        )
      )
      case Accumulate(value) => mod(ms =>
        ms.copy(
          accumulator = ms.accumulator + value,
          visited = ms.visited + ms.instructionPointer,
          instructionPointer = ms.instructionPointer + 1
        )
      )
      case Jump(value)       => mod(ms =>
        ms.copy(
          visited = ms.visited + ms.instructionPointer,
          instructionPointer = ms.instructionPointer + value
        )
      )
    }) *> exec
  } yield {

  }
}