package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends Challenge {

  def route(wire: List[String]): Set[(Int, Int)] = {

    def project(dir: Char, dist: Int, start: (Int, Int)): Seq[(Int, Int)] = dir match {
      case 'U' => (start._2 + 1 to start._2 + dist).map(i => (start._1, i))
      case 'D' => (start._2 - 1 to start._2 - dist by -1).map(i => (start._1, i))
      case 'R' => (start._1 + 1 to start._1 + dist).map(i => (i, start._2))
      case 'L' => (start._1 - 1 to start._1 - dist by -1).map(i => (i, start._2))
    }

    @tailrec
    def acc(p: (Int, Int), xs: List[String], points: Set[(Int, Int)]): Set[(Int, Int)] = xs match {
      case h :: t =>
        val path = project(h.head, h.drop(1).toInt, p)
        acc(path.last, t, points ++ path)
      case _ => points
    }

    acc((0, 0), wire, Set())
  }

  override def run(): Any = {
    val input          = Source.fromResource("day3.txt").getLines.map(_.split(",").toList).toList
    val (wire1, wire2) = (input.head, input.last)
    route(wire1).intersect(route(wire2)).map(i => i._1.abs + i._2.abs).min
  }

}
