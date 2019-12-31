package challenge

import base.Challenge

import scala.io.Source

object Day8b extends Challenge {

  override def run(): Any = {
    val input: List[Char]        = Source.fromResource("day8.txt").mkString.trim.toList
    val layers: List[List[Char]] = input.grouped(25 * 6).toList
    val pic                      = layers.transpose.map(_.dropWhile(_ == '2').head).grouped(25).map(_.mkString)
    pic.map(_ :+ '\n').mkString.replace('1', '█').replace('0', ' ')
  }

}
