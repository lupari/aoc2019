package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day2b extends Challenge {

  def execute(program: List[Int]): List[Int] = {

    @tailrec
    def acc(p: Int, xs: List[Int]): List[Int] = p match {
      case x if x > xs.length - 1 || xs(x) == 99 => xs
      case x if x > xs.length - 4                => xs
      case _ =>
        val (a, b) = (xs(xs(p + 1)), xs(xs(p + 2)))
        val value  = if (xs(p) == 1) a + b else a * b
        acc(p + 4, xs.patch(xs(p + 3), Seq(value), 1))
    }

    acc(0, program)
  }

  override def run(): Any = {
    val program: List[Int] =
      Source.fromResource("day2.txt").mkString.split(",").map(_.trim.toInt).toList
    val input: Seq[(Int, Int)] = for { x <- 0 to 99; y <- 0 to 99 } yield (x, y)
    val pair: (Int, Int) = input
      .find(p => execute(program.patch(1, Seq(p._1), 1).patch(2, Seq(p._2), 1)).head == 19690720)
      .get

    100 * pair._1 + pair._2
  }

}
