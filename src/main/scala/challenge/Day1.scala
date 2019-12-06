package challenge

import base.Challenge

import scala.io.Source

object Day1 extends Challenge {

  override def run(): Any =
    Source.fromResource("day1.txt").getLines().map(_.toInt).map(_ / 3 - 2).sum

}
