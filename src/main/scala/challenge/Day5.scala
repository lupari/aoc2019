package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends Challenge {

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

    def read(xs: List[Int], param: Int, mode: Int): Int = mode match {
      case 0 => xs(param)
      case 1 => param
    }

    @tailrec
    def acc(p: Int, xs: List[Int], out: List[Int]): List[Int] = xs(p) match {
      case 99 => out
      case instr =>
        val (opcode, m1, m2, _) = parse(instr)
        opcode match {
          case 1 =>
            val sum = read(xs, xs(p + 1), m1) + read(xs, xs(p + 2), m2)
            acc(p + 4, xs.updated(xs(p + 3), sum), out)
          case 2 =>
            val product = read(xs, xs(p + 1), m1) * read(xs, xs(p + 2), m2)
            acc(p + 4, xs.updated(xs(p + 3), product), out)
          case 3 =>
            acc(p + 2, xs.updated(xs(p + 1), id), out)
          case 4 =>
            val output = read(xs, xs(p + 1), m1)
            acc(p + 2, xs, out :+ output)
        }
    }

    acc(0, program, Nil)
  }

  override def run(): Any = {
    val program: List[Int] =
      Source.fromResource("day5.txt").mkString.split(",").map(_.trim.toInt).toList
    execute(program, 1).dropWhile(_ == 0).head
  }

}
