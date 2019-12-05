package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends Challenge {

  def execute(program: List[Int]): List[Int] = {

    def parse(d: Int): (Int, Int, Int, Int) = {
      val opcode = d % 100
      val params = d / 100
      val p1     = params % 10
      val p2 = params match {
        case x if x >= 100 => params / 10 % 10
        case x if x >= 10  => params / 10
        case _             => 0
      }
      val p3 = params match {
        case x if x >= 100 => params / 100
        case _             => 0
      }
      (opcode, p1, p2, p3)
    }

    def read(xs: List[Int], param: Int, mode: Int): Int = mode match {
      case 0 => xs(param)
      case 1 => param
    }

    @tailrec
    def acc(p: Int, xs: List[Int], out: List[Int]): List[Int] = xs(p) match {
      case 99 => out
      case x =>
        val instr = parse(x)
        instr._1 match {
          case 1 =>
            val sum = read(xs, xs(p + 1), instr._2) + read(xs, xs(p + 2), instr._3)
            acc(p + 4, xs.patch(xs(p + 3), Seq(sum), 1), out)
          case 2 =>
            val product = read(xs, xs(p + 1), instr._2) * read(xs, xs(p + 2), instr._3)
            acc(p + 4, xs.patch(xs(p + 3), Seq(product), 1), out)
          case 3 =>
            acc(p + 2, xs.patch(xs(p + 1), Seq(1), 1), out)
          case 4 =>
            val output = read(xs, xs(p + 1), instr._2)
            acc(p + 2, xs, out :+ output)
        }
    }

    acc(0, program, Nil)
  }

  override def run(): Any = {
    val program: List[Int] =
      Source.fromResource("day5.txt").mkString.split(",").map(_.trim.toInt).toList
    execute(program).dropWhile(_ == 0).head
  }

}
