package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day17 extends Challenge {

  case class Point(x: Int, y: Int) {
    def neighbors = List(Point(x, y + 1), Point(x, y - 1), Point(x + 1, y), Point(x - 1, y))
  }

  def getGrid(input: List[Int]): Map[Point, Char] = {

    def acc(xs: List[Int], grid: Map[Point, Char], current: Point): Map[Point, Char] = xs match {
      case h :: t =>
        h.toChar match {
          case '\n' =>
            acc(t, grid, Point(0, current.y + 1))
          case _ =>
            acc(t, grid.updated(current, h.toChar), Point(current.x + 1, current.y))
        }
      case _ => grid
    }

    acc(input, Map.empty.withDefaultValue('.'), Point(0, 0))
  }

  def isIntersection(p: Point, grid: Map[Point, Char]): Boolean = p.neighbors.forall(grid(_) == '#')

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day17.txt"))
    val output  = ic.execute(ic.Input(program, Nil)).sig.map(_.toInt)

    val grid = getGrid(output)

    val intersections: Map[Point, Char] =
      grid.filter(_._2 == '#').filter(p => isIntersection(p._1, grid))

    intersections.keys.toList.map(p => p.x * p.y).sum

  }

}
