package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends Challenge {

  def play(program: ic.Program, walkthrough: List[String]): Int = {

    @tailrec
    def acc(in: ic.Input, moves: List[String], out: List[Int], res: Int): Int = {
      val output = ic.execute(in)
      output match {
        case ic.Output(sig, ic.KILL, _, _) =>
          val pwd = out.map(_.toChar).dropWhile(!_.isDigit).takeWhile(_.isDigit)
          pwd.mkString.toInt
        case ic.Output(Nil, ptr, rb, state) =>
          // uncomment for interactive game play
          // println((out.map(_.toChar)).mkString)
          // val input = StdIn.readLine().trim.toList :+ '\n'
          val input = moves.head.toList :+ '\n'
          acc(ic.Input(state, input.map(_.toInt), Some(ic.Resume(ptr, rb))), moves.tail, Nil, res)
        case ic.Output(sig, ptr, rb, state) =>
          acc(ic.Input(state, Nil, Some(ic.Resume(ptr, rb))), moves, out ++ sig.map(_.toInt), res)
      }
    }
    acc(ic.Input(program, Nil, Some(ic.Resume(0, 0))), walkthrough, Nil, 0)
  }

  override def run(): Any = {
    // Solved by playing the game interactively,
    // might want to automate solution finding at some point
    val walkthrough = List(
      "south",
      "take fuel cell",
      "north",
      "north",
      "west",
      "south",
      "take planetoid",
      "west",
      "take antenna",
      "east",
      "east",
      "take mutex",
      "south",
      "south",
      "east",
      "north"
    )
    val program = ic.read(Source.fromResource("day25.txt"))
    play(program, walkthrough)
  }

}
