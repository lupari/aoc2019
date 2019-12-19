package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day19b extends Challenge {

  case class Point(x: Int, y: Int) {
    def toInput: List[Int] = List(x, y)
  }

  type Grid = Map[Point, Char]

  def findPoint(program: ic.Program, size: Int): Point = {

    def beginning(y: Int): Int = {
      Iterator
        .iterate((size, 0))(x => {
          (x._1 + 1, ic.execute(ic.Input(program, List(x._1, y))).sig.head.toInt)
        })
        .dropWhile(_._2 == 0)
        .next
        ._1 - 1
    }

    val (x, y) = Iterator
      .iterate((size * 2, 0, (0, 0)))(y => {
        val x   = beginning(y._1)
        val out = ic.execute(ic.Input(program, List(x + size - 1, y._1 - size + 1))).sig.head.toInt
        (y._1 + 1, out, (x, y._1))
      })
      .dropWhile(_._2 == 0)
      .next
      ._3

    Point(x, y - size + 1)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day19.txt"))
    val point   = findPoint(program, 100)
    point.x * 10000 + point.y
  }

}
