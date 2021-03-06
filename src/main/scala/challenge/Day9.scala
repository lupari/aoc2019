package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.io.Source

object Day9 extends Challenge {

  def execute(program: ic.Program, inputs: List[Long]): ic.Output = {
    ic.execute(ic.Input(program, inputs))
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day9.txt"))
    execute(program, List(1)).sig.head
  }

}
