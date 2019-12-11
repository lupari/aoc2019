package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day11b extends Challenge {

  case class Point(x: Int, y: Int)
  case class Dir(p: Point, dir: Char) {
    def rotate(clockwise: Boolean = true): Dir = dir match {
      case 'U' => if (clockwise) Dir(Point(p.x + 1, p.y), 'R') else Dir(Point(p.x - 1, p.y), 'L')
      case 'D' => if (clockwise) Dir(Point(p.x - 1, p.y), 'L') else Dir(Point(p.x + 1, p.y), 'R')
      case 'L' => if (clockwise) Dir(Point(p.x, p.y - 1), 'U') else Dir(Point(p.x, p.y + 1), 'D')
      case 'R' => if (clockwise) Dir(Point(p.x, p.y + 1), 'D') else Dir(Point(p.x, p.y - 1), 'U')
    }
  }

  def paint(program: ic.Program): Map[Point, Char] = {

    val grid: Map[Point, Char] = Map(Point(0, 0) -> '#').withDefaultValue('.')

    @tailrec
    def acc(grid: Map[Point, Char], dir: Dir, out: List[Int], in: ic.Input): Map[Point, Char] =
      ic.execute(in) match {
        case ic.Output(_, ic.KILL, _, _) => grid
        case ic.Output(sig, ptr, rb, state) =>
          out match {
            case h :: _ => // ready to serve input
              val color     = if (h == 0) '.' else '#'
              val next      = dir.rotate(clockwise = sig.head == 1)
              val nextColor = if (grid(next.p) == '.') 0 else 1
              val input     = ic.Input(state, List(nextColor), Some(ic.Resume(ptr, rb)))
              acc(grid.updated(dir.p, color), next, Nil, input)
            case _ =>
              val color = sig.head.toInt
              val input = ic.Input(state, Nil, Some(ic.Resume(ptr, rb)))
              acc(grid, dir, List(color), input)
          }
      }

    acc(grid, Dir(Point(0, 0), 'U'), Nil, ic.Input(program, List(1), Some(ic.Resume(0, 0))))
  }

  override def run(): Any = {
    val program                = ic.read(Source.fromResource("day11.txt"))
    val paintJob               = paint(program)
    val (x, y)                 = (paintJob.keys.maxBy(_.x).x, paintJob.keys.maxBy(_.y).y)
    val canvas                 = Array.tabulate(y + 1, x + 1)((_, _) => ' ')
    for { p <- paintJob } yield canvas(p._1.y)(p._1.x) = if (p._2 == '#') '█' else ' '
    canvas
  }

}
