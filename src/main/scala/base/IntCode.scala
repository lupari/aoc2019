package base

import scala.io.Source
import scala.annotation.tailrec

object IntCode {

  type Program = Map[Long, Long]
  case class Resume(pointer: Long, rb: Long)
  case class Input(program: Program, in: List[Long], resume: Option[Resume] = None)
  case class Output(sig: List[Long], p: Long, rb: Long, state: Program)
  val KILL: Long = -1

  def read(src: Source): Program =
    src.mkString
      .split(",")
      .map(_.trim.toLong)
      .zipWithIndex
      .map(x => x._2.toLong -> x._1)
      .toMap
      .withDefaultValue(0)

  def execute(input: Input): Output = {

    def parse(d: Int): (Int, Int, Int, Int) =
      (d % 100, d / 100 % 10, d / 1000 % 10, d / 10000 % 10)

    def index(xs: Program, index: Long, rb: Long, mode: Int): Long = mode match {
      case 0 => xs(index)
      case 1 => index
      case 2 => rb + xs(index)
    }

    def value(xs: Program, i: Long, rb: Long, mode: Int) = xs(index(xs, i, rb, mode))

    @tailrec
    def acc(p: Long, xs: Program, in: List[Long], rb: Long, out: List[Long]): Output =
      xs(p) match {
        case 99 => Output(out, KILL, rb, xs)
        case instr =>
          val (opcode, m1, m2, m3) = parse(instr.toInt)
          val v1                   = value(xs, p + 1, rb, m1)
          val v2                   = value(xs, p + 2, rb, m2)
          val v3                   = index(xs, p + 3, rb, m3)
          opcode match {
            case 1 => // sum
              acc(p + 4, xs.updated(v3, v1 + v2), in, rb, out)
            case 2 => // mul
              acc(p + 4, xs.updated(v3, v1 * v2), in, rb, out)
            case 3 =>
              in match { // read
                case h :: t =>
                  val i = index(xs, p + 1, rb, m1)
                  acc(p + 2, xs.updated(i, h.toLong), t, rb, out)
                case _ => Output(out, p, rb, xs) // halt as no input available
              }
            case 4 => // write
              input.resume match {
                case Some(_) => Output(List(v1), p + 2, rb, xs)
                case None    => acc(p + 2, xs, in, rb, out :+ v1)
              }
            case 5 => // jmp-true
              acc(if (v1 != 0) v2 else p + 3, xs, in, rb, out)
            case 6 => // jmp-false
              acc(if (v1 == 0) v2 else p + 3, xs, in, rb, out)
            case 7 => // lt
              acc(p + 4, xs.updated(v3, if (v1 < v2) 1 else 0), in, rb, out)
            case 8 => // eq
              acc(p + 4, xs.updated(v3, if (v1 == v2) 1 else 0), in, rb, out)
            case 9 => // rb
              acc(p + 2, xs, in, rb + v1, out)
          }
      }

    val pointer: Long = input.resume.map(_.pointer).getOrElse(0)
    val rb: Long      = input.resume.map(_.rb).getOrElse(0)
    acc(pointer, input.program, input.in, rb, Nil)
  }

}
