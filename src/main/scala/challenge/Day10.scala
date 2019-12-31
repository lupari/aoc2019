package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends Challenge {

  case class Point(x: Int, y: Int) {
    def visibleFrom(xs: List[Point]): Set[Point] = {

      @tailrec
      def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

      xs.map(p => {
          val (dx, dy) = (p.x - x, p.y - y)
          val g        = gcd(dx, dy).abs
          Point(dx / g, dy / g)
        })
        .toSet
    }
  }

  override def run(): Any = {
    val grid: List[List[Char]] = Source.fromResource("day10.txt").getLines.map(_.toList).toList
    val asteroids =
      (for { x <- grid.indices; y <- grid.indices if grid(x)(y) == '#' } yield Point(x, y)).toList
    asteroids.map(a => a.visibleFrom(asteroids.diff(List(a)))).map(_.size).max
  }

}
