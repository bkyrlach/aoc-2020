package day8

import cats._
import cats.data.StateT
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import day8.Computer2._
import utils.InputHelper

object Part2 extends IOApp {

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
      case "nop" => NoOp(value)
    }
  }

  def run(args: List[String]): IO[ExitCode] = for {
    inputs  <- InputHelper.read("./inputs/day8/day8.txt")
    program =  inputs.map(input => instruction.runA(input.toList)).sequence match {
      case Right(instructions) => instructions
      case Left(_)             => Nil
    }
    result  =  runProgramWithECC(program)
    _       <- IO { println(s"Accumulator state from corrected program $result.") }
  } yield {
    ExitCode.Success
  }

}

object Computer2 {
  sealed trait Instruction
  case class Accumulate(value: Int) extends Instruction
  case class Jump(value: Int) extends Instruction
  case class NoOp(value: Int) extends Instruction

  case class MachineState(instructionPointer: Int, accumulator: Int, program: Vector[Instruction], visited: Set[Int])

  type Result[A] = Either[String,A]
  type Program[A] = StateT[Result,MachineState,A]

  val get: Program[MachineState] = StateT.get[Result,MachineState]
  def pure[A](a: A): Program[A] = StateT.pure[Result,MachineState,A](a)
  def inspect[A](view: MachineState => A): Program[A] = StateT.inspect[Result,MachineState,A](view)
  def mod(update: MachineState => MachineState): Program[Unit] = StateT.modify[Result,MachineState](update)
  def liftF[A](result: Result[A]): Program[A] = StateT.liftF[Result,MachineState,A](result)
  def err[A](msg: String): Program[A] = liftF(Left(msg))

  def fixInstruction(program: List[Instruction], skip: Int): Result[List[Instruction]] = program match {
    case Nil  => if(skip > 0) Left("Reached end of program") else Right(Nil)
    case h::t => h match {
      case Jump(value) => if(skip > 0) fixInstruction(t, skip - 1).map(h::_) else Right(NoOp(value)::t)
      case NoOp(value) => if(skip > 0) fixInstruction(t, skip - 1).map(h::_) else Right(Jump(value)::t)
      case _ => fixInstruction(t, skip - 1).map(h::_)
    }
  }

  def runProgramWithECC(program: List[Instruction]): Result[Int] = {
    def go(currentProgram: List[Instruction], skip: Int): Result[Int] = {
      fixInstruction(currentProgram, skip).flatMap(p => runProgram(p.toVector)) match {
        case Right(value) => Right(value)
        case Left(_)      => go(program, skip + 1)
      }
    }
    go(program, 0)
  }

  def runProgram(program: Vector[Instruction]): Result[Int] = {
    val initState = MachineState(
      instructionPointer = 0,
      accumulator = 0,
      program = program,
      visited = Set.empty[Int]
    )
    exec.runS(initState).map(_.accumulator)
  }

  val noop: Program[Unit] = mod(ms =>
    ms.copy(
      visited = ms.visited + ms.instructionPointer,
      instructionPointer = ms.instructionPointer + 1
    )
  )

  def acc(value: Int): Program[Unit] = mod(ms =>
    ms.copy(
      accumulator = ms.accumulator + value,
      visited = ms.visited + ms.instructionPointer,
      instructionPointer = ms.instructionPointer + 1
    )
  )

  def jmp(value: Int): Program[Unit] = mod(ms =>
    ms.copy(
      visited = ms.visited + ms.instructionPointer,
      instructionPointer = ms.instructionPointer + value
    )
  )

  val exec: Program[Unit] = for {
    isLoop       <- inspect(ms => ms.visited.contains(ms.instructionPointer))
    _            <- if(isLoop) err("Loop detected") else pure(())
    endOfProgram <- inspect(ms => ms.instructionPointer >= ms.program.size)
    _            <- if(endOfProgram) pure(()) else (inspect(ms => ms.program(ms.instructionPointer)) >>= {
      case NoOp(value)       => noop
      case Accumulate(value) => acc(value)
      case Jump(value)       => jmp(value)
    }) *> exec
  } yield {

  }
}