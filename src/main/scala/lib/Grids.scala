package lib

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Grids {

  case class Point(x: Int, y: Int) {
    def +(p: Point): Point = Point(x + p.x, y + p.y)
    def neighbors          = List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))
    def corners =
      List(Point(x - 1, y - 1), Point(x + 1, y - 1), Point(x - 1, y + 1), Point(x + 1, y + 1))
  }
  object Point {
    def adj: List[Point] = List(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
  }

  case class Dir(p: Point, dir: Char) {
    def forward(): Dir = dir match {
      case 'U' => copy(p = p.copy(y = p.y - 1))
      case 'D' => copy(p = p.copy(y = p.y + 1))
      case 'L' => copy(p = p.copy(x = p.x - 1))
      case 'R' => copy(p = p.copy(x = p.x + 1))
    }
    def rotate(clockwise: Boolean): Dir = dir match {
      case 'U' => if (clockwise) Dir(Point(p.x + 1, p.y), 'R') else Dir(Point(p.x - 1, p.y), 'L')
      case 'D' => if (clockwise) Dir(Point(p.x - 1, p.y), 'L') else Dir(Point(p.x + 1, p.y), 'R')
      case 'L' => if (clockwise) Dir(Point(p.x, p.y - 1), 'U') else Dir(Point(p.x, p.y + 1), 'D')
      case 'R' => if (clockwise) Dir(Point(p.x, p.y + 1), 'D') else Dir(Point(p.x, p.y - 1), 'U')
    }
  }

  type Grid[A] = Map[Point, A]

  object GridInput {
    def apply(input: List[Char]): Grid[Char] = {
      @tailrec
      def acc(xs: List[Char], grid: Grid[Char], current: Point): Grid[Char] =
        xs match {
          case h :: t if h == '\n' => acc(t, grid, Point(0, current.y + 1))
          case h :: t              => acc(t, grid.updated(current, h), Point(current.x + 1, current.y))
          case _                   => grid
        }
      acc(input, Map.empty, Point(0, 0))
    }
  }

  object GridCanvas {
    def apply[A](grid: Grid[A], default: A)(cf: A => A)(
        implicit classTag: ClassTag[A]): Array[Array[A]] = {
      val (x, y) = (grid.keys.maxBy(_.x).x, grid.keys.maxBy(_.y).y)
      val canvas = Array.tabulate(y + 1, x + 1)((_, _) => default)
      for { p <- grid } yield canvas(p._1.y)(p._1.x) = cf(p._2)
      canvas
    }

  }

}
