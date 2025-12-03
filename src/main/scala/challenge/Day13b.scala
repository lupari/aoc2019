package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day13b extends Challenge {

  case class Game(ball: Int = 0, paddle: Int = 0, score: Int = 0)

  def play(program: ic.Program): Int = {

    @tailrec
    def acc(in: ic.Input, out: List[Int], game: Game): Int = {
      val output = ic.execute(in)
      val input  = ic.Input(output.state, Nil, Some(ic.State(output.p, output.rb)))
      output match {
        case ic.Output(_, ic.KILL, _, _) => game.score // game over
        case ic.Output(Nil, _, _, _) => // input wanted
          val move = game.ball.compare(game.paddle)
          acc(input.copy(in = List(move)), Nil, game)
        case ic.Output(sig, _, _, _) =>
          out match {
            case h :: i :: _ if h == -1 && i == 0 => // score
              acc(input, Nil, game.copy(score = sig.head.toInt))
            case h :: _ :: _ => // tile type received
              val ball   = if (sig.head == 4) h else game.ball
              val paddle = if (sig.head == 3) h else game.paddle
              acc(input, Nil, game.copy(ball = ball, paddle = paddle))
            case _ => // coordinates received
              acc(input, out :+ sig.head.toInt, game)
          }
      }
    }
    acc(ic.Input(program, Nil, Some(ic.State(0, 0))), Nil, Game())
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day13.txt"))
    play(program.updated(0, 2))
  }

}
