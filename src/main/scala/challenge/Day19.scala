package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day19 extends Challenge {

  def beamSize(program: ic.Program, points: Seq[(Int, Int)]): Int =
    points.foldLeft(0)((size, p) => {
      val input = ic.Input(program, List(p._1, p._2))
      size + ic.execute(input).sig.head.toInt
    })

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day19.txt"))
    val input   = for { y <- (0 until 50); x <- (0 until 50) } yield (x, y)
    beamSize(program, input)
  }

}
