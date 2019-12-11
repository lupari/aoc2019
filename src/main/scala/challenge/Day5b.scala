package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day5b extends Challenge {

  def execute(program: ic.Program, id: Int): ic.Output = ic.execute(ic.Input(program, List(5)))

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day5.txt"))
    execute(program, id = 5).sig.head
  }

}
