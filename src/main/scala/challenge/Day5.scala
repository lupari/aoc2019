package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day5 extends Challenge {

  def execute(program: ic.Program, id: Int): List[Long] =
    ic.execute(ic.Input(program, List(id))).sig

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day5.txt"))
    execute(program, 1).dropWhile(_ == 0).head
  }

}
