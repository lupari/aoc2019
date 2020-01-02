package challenge

import base.Challenge
import intcode.{IntCode => ic}
import lib.GridImplicits._
import lib.Points._

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Challenge {

  def paint(program: ic.Program): Grid[Char] = {

    @tailrec
    def acc(grid: Grid[Char], dir: Dir, out: List[Int], in: ic.Input): Grid[Char] =
      ic.execute(in) match {
        case ic.Output(_, ic.KILL, _, _) => grid
        case ic.Output(sig, ptr, rb, state) =>
          out match {
            case h :: _ => // ready to serve input
              val color     = if (h == 0) '.' else '#'
              val next      = dir.rotate(clockwise = sig.head == 1)
              val nextColor = if (grid(next.p) == '.') 0 else 1
              val input     = ic.Input(state, List(nextColor), Some(ic.State(ptr, rb)))
              acc(grid.updated(dir.p, color), next, Nil, input)
            case _ =>
              val color = sig.head.toInt
              val input = ic.Input(state, Nil, Some(ic.State(ptr, rb)))
              acc(grid, dir, List(color), input)
          }
      }

    val grid = Map(Point(0, 0) -> '.').withDefaultValue('.')
    acc(grid, Dir(Point(0, 0), 'U'), Nil, ic.Input(program, List(0), Some(ic.State(0, 0))))
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day11.txt"))
    paint(program).size
  }

}
