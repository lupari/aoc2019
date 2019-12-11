package challenge

import base.Challenge
import base.{IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Challenge {

  case class Point(x: Int, y: Int)

  def paint(program: Map[Long, Long], grid: Map[Point, Char]): Map[Point, Char] = {

    case class ProgramInput(prog: Map[Long, Long], ip: Long, rb: Long, in: List[Int])

    def direction(currentDir: Char, turn: Char): Char = currentDir match {
      case 'U' => if (turn == 'L') 'L' else 'R'
      case 'D' => if (turn == 'L') 'R' else 'L'
      case 'L' => if (turn == 'L') 'D' else 'U'
      case 'R' => if (turn == 'L') 'U' else 'D'
    }

    def location(p: Point, dir: Char): Point = dir match {
      case 'U' => Point(p.x, p.y + 1)
      case 'D' => Point(p.x, p.y - 1)
      case 'L' => Point(p.x - 1, p.y)
      case 'R' => Point(p.x + 1, p.y)
    }

    @tailrec
    def acc(grid: Map[Point, Char],
            p: Point,
            dir: Char,
            buf: List[Int],
            input: ProgramInput): Map[Point, Char] =
      ic.execute(input.prog, input.in, input.ip, feedbackMode = true) match {
        case ic.Output(_, ic.KILL, _, _) => grid
        case ic.Output(sig, ptr, rb, state) =>
          buf match {
            case h :: t => // output buf initialized
              val color     = if (h == 0) '.' else '#'
              val turn      = if (sig.head == 0) 'L' else 'R'
              val nextDir   = direction(dir, turn)
              val nextPoint = location(p, nextDir)
              val nextColor = if (grid(nextPoint) == '.') 0 else 1
              val input     = ProgramInput(state, ptr, rb, List(nextColor))
              acc(grid.updated(p, color), nextPoint, nextDir, Nil, input)
            case _ => // output buf empty
              val color = sig.head.toInt
              val input = ProgramInput(state, ptr, rb, Nil)
              acc(grid, p, dir, List(color), input)
          }
      }

    acc(grid, Point(0, 0), 'U', Nil, ProgramInput(program, 0, 0, List(0)))

  }

  override def run(): Any = {
    val input = Source.fromResource("day11.txt").mkString.split(",").map(_.trim.toLong).toList
    val program: Map[Long, Long] =
      input.zipWithIndex.map(x => x._2.toLong -> x._1).toMap.withDefaultValue(0)
    val grid: Map[Point, Char] = Map(Point(0, 0) -> '.').withDefaultValue('.')
    paint(program, grid).size
  }

}
