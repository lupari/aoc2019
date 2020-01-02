package challenge

import base.Challenge
import lib.Points.Point

import scala.annotation.tailrec
import scala.io.Source
import scala.Ordering.Double.TotalOrdering

object Day10b extends Challenge {

  def visibleFrom(p: Point, xs: List[Point]): Set[Point] = {
    @tailrec
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    xs.map(p2 => {
        val (dx, dy) = (p2.x - p.x, p2.y - p.y)
        val d        = math.abs(gcd(dx, dy))
        Point(dx / d, dy / d)
      })
      .toSet
  }

  override def run(): Any = {
    val grid: List[List[Char]] = Source.fromResource("day10.txt").getLines.map(_.toList).toList
    val asteroids =
      (for { x <- grid.indices; y <- grid.indices if grid(x)(y) == '#' } yield Point(x, y)).toList
    val (home, targets) =
      asteroids.map(a => (a, visibleFrom(a, asteroids.diff(List(a))).toList)).maxBy(_._2.size)
    val target = home + targets
      .map(a => (math.atan2(a.y, a.x), a))
      .sortBy({ case (a, Point(x, y)) => (a, x, y) })
      .reverse
      .drop(199)
      .head
      ._2
    target.y * 100 + target.x
  }

}
