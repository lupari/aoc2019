package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day3b extends Challenge {

  def route(wire: List[String]): Seq[(Int, Int)] = {

    def project(dir: Char, dist: Int, start: (Int, Int)): Seq[(Int, Int)] = dir match {
      case 'U' => (start._2 + 1 to start._2 + dist).map(i => (start._1, i))
      case 'D' => (start._2 - 1 to start._2 - dist by -1).map(i => (start._1, i))
      case 'R' => (start._1 + 1 to start._1 + dist).map(i => (i, start._2))
      case 'L' => (start._1 - 1 to start._1 - dist by -1).map(i => (i, start._2))
    }

    @tailrec
    def acc(p: (Int, Int), xs: List[String], coords: Seq[(Int, Int)]): Seq[(Int, Int)] = xs match {
      case h :: t =>
        val path = project(h.head, h.drop(1).toInt, p)
        acc(path.last, t, coords ++ path)
      case _ => coords
    }

    Seq((0, 0)) ++ acc((0, 0), wire, Nil)
  }

  override def run(): Any = {
    val input          = Source.fromResource("day3.txt").getLines.map(s => s.split(",").toList).toList
    val (wire1, wire2) = (input.head, input.last)
    val route1         = route(wire1)
    val route2         = route(wire2)
    route1.intersect(route2).drop(1).map(i => route1.indexOf(i) + route2.indexOf(i)).min
  }

}
