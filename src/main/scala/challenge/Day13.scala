package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day13 extends Challenge {

  case class Point(x: Int, y: Int)

  def play(program: ic.Program): Map[Point, Char] = {

    def interpret(x: Int, y: Int, o: Int): (Point, Char) = {
      (Point(x, y), o match {
        case 0 => ' '
        case 1 => '#'
        case 2 => 'X'
        case 3 => 'P'
        case 4 => 'o'
      })
    }

    ic.execute(ic.Input(program, Nil))
      .sig
      .map(_.toInt)
      .grouped(3)
      .map({ case List(a, b, c) => interpret(a, b, c) })
      .map(x => x._1 -> x._2)
      .toMap
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day13.txt"))
    play(program).values.count(_ == 'X')
  }

}
