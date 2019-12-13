package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day13 extends Challenge {

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day13.txt"))
    ic.execute(ic.Input(program, Nil))
      .sig
      .grouped(3)
      .map(_.last)
      .count(_ == 2)
  }

}
