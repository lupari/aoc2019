package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends Challenge {

  case class Point(x: Int, y: Int)

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def visible(station: Point, asteroids: List[Point]) = {
    asteroids
      .map(a => {
        val (dx, dy) = (a.x - station.x, a.y - station.y)
        val g        = math.abs(gcd(dx, dy))
        Point(dx / g, dy / g)
      })
      .toSet
  }

  override def run(): Any = {
    val grid: List[List[Char]] = Source.fromResource("day10.txt").getLines.map(_.toList).toList
    val asteroids = (for { x <- grid.indices; y <- grid.indices if grid(y)(x) == '#' } yield
      Point(x, grid.length - 1 - y)).toList
    asteroids.map(a => visible(a, asteroids.diff(List(a)))).map(_.size).max
  }

}
