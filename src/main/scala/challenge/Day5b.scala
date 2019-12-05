package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day5b extends Challenge {

  def execute(program: List[Int], id: Int): List[Int] = {

    def parse(d: Int): (Int, Int, Int, Int) = {
      val opcode = d % 100
      val params = d / 100
      val p1     = params % 10
      val p2 = params match {
        case x if x >= 100 => x / 10 % 10
        case x if x >= 10  => x / 10
        case _             => 0
      }
      val p3 = if (params >= 100) params / 100 else 0
      (opcode, p1, p2, p3)
    }

    def value(xs: List[Int], index: Int, mode: Int): Int =
      if (mode == 0) xs(xs(index)) else xs(index)

    @tailrec
    def acc(p: Int, xs: List[Int], out: List[Int]): List[Int] = xs(p) match {
      case 99 => out
      case instr =>
        val (opcode, m1, m2, _) = parse(instr)
        val v1                  = value(xs, p + 1, m1)
        val v2                  = value(xs, p + 2, m2)
        opcode match {
          case 1 => // sum
            acc(p + 4, xs.updated(xs(p + 3), v1 + v2), out)
          case 2 => // mul
            acc(p + 4, xs.updated(xs(p + 3), v1 * v2), out)
          case 3 => // read
            acc(p + 2, xs.updated(xs(p + 1), id), out)
          case 4 => // write
            acc(p + 2, xs, out :+ v1)
          case 5 => // jmp-true
            acc(if (v1 != 0) v2 else p + 3, xs, out)
          case 6 => // jmp-false
            acc(if (v1 == 0) v2 else p + 3, xs, out)
          case 7 => // eq
            acc(p + 4, xs.updated(xs(p + 3), if (v1 < v2) 1 else 0), out)
          case 8 => // lt
            acc(p + 4, xs.updated(xs(p + 3), if (v1 == v2) 1 else 0), out)
        }
    }

    acc(0, program, Nil)
  }

  override def run(): Any = {
    val program = Source.fromResource("day5.txt").mkString.split(",").map(_.trim.toInt).toList
    execute(program, id = 5).head
  }

}
