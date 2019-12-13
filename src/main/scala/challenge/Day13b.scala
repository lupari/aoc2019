package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day13b extends Challenge {

  case class Point(x: Int, y: Int)

  def play(program: ic.Program): Int = {

    def panel(i: Int): Char = i match {
      case 0 => ' '
      case 1 => '#'
      case 2 => 'X'
      case 3 => 'P'
      case 4 => 'o'
    }

    def nextMove(b: Int, p: Int): Int = p match {
      case x if x > b  => -1
      case x if x < b  => 1
      case x if x == b => 0
    }

    @tailrec
    def acc(xs: Map[Point, Char], out: List[Int], in: ic.Input, ctrl: (Int, Int), score: Int): Int =
      ic.execute(in) match {
        case ic.Output(_, ic.KILL, _, _) => score // game over
        case ic.Output(Nil, ptr, rb, state) => // input wanted
          val move  = nextMove(ctrl._1, ctrl._2)
          val input = ic.Input(state, List(move), Some(ic.Resume(ptr, rb)))
          acc(xs, Nil, input, ctrl, score)
        case ic.Output(sig, ptr, rb, state) =>
          out match {
            case h :: i :: _ if (h == -1 && i == 0) => // score
              val input = ic.Input(state, Nil, Some(ic.Resume(ptr, rb)))
              acc(xs, Nil, input, ctrl, sig.head.toInt)
            case h :: i :: _ => // display buffer full
              val item  = panel(sig.head.toInt)
              val ballX = if (item == 'o') h else ctrl._1
              val padX  = if (item == 'P') h else ctrl._2
              val input = ic.Input(state, Nil, Some(ic.Resume(ptr, rb)))
              acc(xs.updated(Point(h, i), item), Nil, input, (ballX, padX), score)
            case _ => // display buffer event
              val input = ic.Input(state, Nil, Some(ic.Resume(ptr, rb)))
              acc(xs, out :+ sig.head.toInt, input, ctrl, score)
          }
      }

    val grid: Map[Point, Char] = Map().withDefaultValue(' ')
    acc(grid, Nil, ic.Input(program, Nil, Some(ic.Resume(0, 0))), (-1, -1), 0)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day13.txt"))
    play(program.updated(0, 2))
  }

}
