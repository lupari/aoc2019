package base

import scala.annotation.tailrec

object IntCode {

  case class Output(sig: List[Long], p: Long, rb: Long, state: Map[Long, Long])
  val KILL: Long = -1

  def execute(program: Map[Long, Long],
              inputs: List[Int],
              pointer: Long = 0,
              rb: Long = 0,
              feedbackMode: Boolean = false): Output = {

    def parse(d: Int): (Int, Int, Int, Int) =
      (d % 100, d / 100 % 10, d / 1000 % 10, d / 10000 % 10)

    def index(xs: Map[Long, Long], index: Long, rb: Long, mode: Int): Long = mode match {
      case 0 => xs(index)
      case 1 => index
      case 2 => rb + xs(index)
    }

    def value(xs: Map[Long, Long], i: Long, rb: Long, mode: Int) = xs(index(xs, i, rb, mode))

    @tailrec
    def acc(p: Long, xs: Map[Long, Long], in: List[Int], rb: Long, out: List[Long]): Output =
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
              if (feedbackMode) Output(List(v1), p + 2, rb, xs)
              else acc(p + 2, xs, in, rb, out :+ v1)
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

    acc(pointer, program, inputs, rb, Nil)
  }

}
