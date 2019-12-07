package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends Challenge {

  def execute(program: List[Int], inputs: List[Int]): List[Int] = {

    def parse(d: Int): (Int, Int, Int, Int) =
      (d % 100, d / 100 % 10, d / 1000 % 10, d / 10000 % 10)

    def value(xs: List[Int], index: Int, mode: Int): Int =
      if (mode == 0) xs(xs(index)) else xs(index)

    @tailrec
    def acc(p: Int, xs: List[Int], in: List[Int], out: List[Int]): List[Int] = xs(p) match {
      case 99 => out
      case 3 => acc(p + 2, xs.updated(xs(p + 1), in.head), in.drop(1), out)
      case instr =>
        val (opcode, m1, m2, _) = parse(instr)
        val v1                  = value(xs, p + 1, m1)
        val v2                  = value(xs, p + 2, m2)
        opcode match {
          case 1 => // sum
            acc(p + 4, xs.updated(xs(p + 3), v1 + v2), in, out)
          case 2 => // mul
            acc(p + 4, xs.updated(xs(p + 3), v1 * v2), in, out)
          case 4 => // write
            acc(p + 2, xs, in, out :+ v1)
          case 5 => // jmp-true
            acc(if (v1 != 0) v2 else p + 3, xs, in, out)
          case 6 => // jmp-false
            acc(if (v1 == 0) v2 else p + 3, xs, in, out)
          case 7 => // eq
            acc(p + 4, xs.updated(xs(p + 3), if (v1 < v2) 1 else 0), in, out)
          case 8 => // lt
            acc(p + 4, xs.updated(xs(p + 3), if (v1 == v2) 1 else 0), in, out)
        }
    }

    acc(0, program, inputs, Nil)
  }

  override def run(): Any = {
    val program = Source.fromResource("day7.txt").mkString.split(",").map(_.trim.toInt).toList
    (0 to 4).permutations.map(
      seq => seq.foldLeft(0)((acc, i) => execute(program, List(i, acc)).head)
    ).max
  }

}
