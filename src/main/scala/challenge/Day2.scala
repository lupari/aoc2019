package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends Challenge {

  def execute(program: List[Int]): List[Int] = {

    @tailrec
    def acc(p: Int, xs: List[Int]): List[Int] = xs(p) match {
      case 99 => xs
      case instr =>
        val (a, b) = (xs(xs(p + 1)), xs(xs(p + 2)))
        val value  = if (instr == 1) a + b else a * b
        acc(p + 4, xs.updated(xs(p + 3), value))
    }

    acc(0, program)
  }

  override def run(): Any = {
    val program: List[Int] =
      Source.fromResource("day2.txt").mkString.split(",").map(_.trim.toInt).toList
    execute(program.patch(1, Seq(12), 1).patch(2, Seq(2), 1)).head
  }

}
