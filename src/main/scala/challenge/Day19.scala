package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day19 extends Challenge {

  case class Point(x: Int, y: Int) {
    def toInput: List[Int] = List(x, y)
  }

  type Grid = Map[Point, Char]

  def beamShape(program: ic.Program, points: List[Point]): Grid = {

    points.foldLeft(Map[Point, Char]())((a, b) => {
      val input = ic.Input(program, b.toInput)
      val sig   = ic.execute(input).sig.head
      a.updated(b, if (sig == 0) ' ' else '#')
    })

  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day19.txt"))
    val input =
      (for { y <- (0 until 50); x <- (0 until 50) } yield
        (x, y)).map(xy => Point(xy._1, xy._2)).toList
    val grid: Grid = beamShape(program, input)
    grid.values.count(_ == '#')
  }

}
