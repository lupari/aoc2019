package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.io.Source

object Day2 extends Challenge {

  def execute(program: ic.Program): ic.Output = ic.execute(ic.Input(program, Nil))

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day2.txt")).updated(1, 12L).updated(2, 2L)
    execute(program).state(0)
  }

}
