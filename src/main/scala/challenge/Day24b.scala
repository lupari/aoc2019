package challenge

import base.Challenge
import lib.Points.Point

import scala.io.Source

object Day24b extends Challenge {

  type PointAtLevel = (Int, Point)
  type Grid         = List[List[Char]]
  type Grids        = Map[Int, Grid]

  def calculate(grid: Grid, minutes: Int = 200): Long = {

    def neighbors(lp: PointAtLevel): List[PointAtLevel] = {
      val (level, point) = lp
      Point.adj
        .flatMap(delta =>
          point + delta match {
            case Point(-1, _) => List((level - 1, Point(1, 2)))
            case Point(5, _)  => List((level - 1, Point(3, 2)))
            case Point(_, -1) => List((level - 1, Point(2, 1)))
            case Point(_, 5)  => List((level - 1, Point(2, 3)))
            case Point(2, 2) =>
              delta match {
                case Point(1, 0)  => List.tabulate(5)(y => (level + 1, Point(0, y)))
                case Point(-1, 0) => List.tabulate(5)(y => (level + 1, Point(4, y)))
                case Point(0, 1)  => List.tabulate(5)(x => (level + 1, Point(x, 0)))
                case Point(0, -1) => List.tabulate(5)(x => (level + 1, Point(x, 4)))
                case _            => throw new NoSuchElementException
              }
            case point => List((level, point))
        })
    }

    def transform(gs: Grids): Grids = {
      val newGrid     = List.fill(5, 5)('.')
      val extraLevels = List((gs.keys.min - 1, newGrid), (gs.keys.max + 1, newGrid))
      (gs ++ extraLevels)
        .map({
          case (level, grid) =>
            (level, grid.zipWithIndex.map({
              case (row, y) =>
                row.zipWithIndex.map({
                  case (col, x) =>
                    Point(x, y) match {
                      case Point(2, 2) => '?'
                      case point =>
                        val adj = neighbors((level, point)).map({
                          case (level, p) => gs.getOrElse(level, newGrid)(p.y)(p.x)
                        })
                        val bc = adj.count(_ == '#')
                        col match {
                          case '#' =>
                            if (bc == 1) '#' else '.'
                          case '.' =>
                            if (bc == 1 || bc == 2) '#' else '.'
                          case c => c
                        }
                    }
                })
            }))
        })
    }
    val afterMinutes = Iterator.iterate(Map(0 -> grid))(transform).drop(minutes).next
    afterMinutes.values.flatten.flatten.count(_ == '#')
  }

  override def run(): Any = {
    val grid = Source.fromResource("day24.txt").getLines.map(_.toList).toList
    calculate(grid)
  }
}
