package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.io.Source

object Day9b extends Challenge {

  def execute(program: Map[Long, Long], inputs: List[Long]): ic.Output =
    ic.execute(ic.Input(program, inputs))

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day9.txt"))
    execute(program, List(2)).sig.head
  }

}
