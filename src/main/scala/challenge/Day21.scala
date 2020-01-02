package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.io.Source

object Day21 extends Challenge {

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day21.txt"))
    // reasoned through trial and error, maybe at some point make some heuristic for automatically generating
    // working input
    val input =
      List("NOT D T", "OR C T", "AND A T", "NOT T J", "WALK")
        .flatMap(_.toList :+ '\n')
        .map(_.toLong)

    val output = ic.execute(ic.Input(program, input))
    // Console.err.println(output.sig.map(_.toChar).mkString)
    output.sig.last.toInt

  }

}
