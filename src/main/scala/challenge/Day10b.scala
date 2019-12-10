package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Ordered.orderingToOrdered

object Day10b extends Challenge {

  case class Point(x: Int, y: Int) extends Ordered[Point] {
    override def compare(p: Point): Int = (x, y).compareTo((p.x, p.y))
    def +(p: Point) = Point(x + p.x, y + p.y)
    def visibleFrom(xs: List[Point]): Set[Point] = {
      @tailrec
      def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

      xs.map(p => {
          val (dx, dy) = (p.x - x, p.y - y)
          val d        = math.abs(gcd(dx, dy))
          Point(dx / d, dy / d)
        })
        .toSet
    }
  }

  override def run(): Any = {
    val grid: List[List[Char]] = Source.fromResource("day10.txt").getLines.map(_.toList).toList
    val asteroids =
      (for { x <- grid.indices; y <- grid.indices if grid(x)(y) == '#' } yield Point(x, y)).toList
    val (home, targets) =
      asteroids.map(a => (a, a.visibleFrom(asteroids.diff(List(a))).toList)).maxBy(_._2.size)
    val target = home + targets.map(a => (math.atan2(a.y, a.x), a)).sorted.reverse(199)._2
    target.y * 100 + target.x
  }

}
