package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day1b extends Challenge {

  def fuelSum(a: Int): Int = {

    @tailrec
    def acc(xs: Int, sum: Int): Int = xs match {
      case x if x > 0 =>
        val amount = Math.max(0, x / 3 - 2)
        acc(amount, sum + amount)
      case _ => sum
    }

    acc(a, 0)
  }

  override def run(): Any = Source.fromResource("day1.txt").getLines().map(_.toInt).map(fuelSum).sum

}
