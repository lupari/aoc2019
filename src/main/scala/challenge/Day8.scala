package challenge

import base.Challenge

import scala.io.Source

object Day8 extends Challenge {

  override def run(): Any = {
    val input: List[Char] = Source.fromResource("day8.txt").mkString.trim.toList
    val layer: List[Char] = input.grouped(25 * 6).minBy(_.count(_ == '0'))
    layer.count(_ == '1') * layer.count(_ == '2')
  }

}
