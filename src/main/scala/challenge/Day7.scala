package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day7 extends Challenge {

  def execute(program: ic.Program, inputs: List[Long]): List[Int] = {
    ic.execute(ic.Input(program, inputs)).sig.map(_.toInt)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day7.txt"))
    (0 to 4).permutations
      .map(
        seq => seq.foldLeft(0)((acc, i) => execute(program, List(i, acc)).head)
      )
      .max
  }

}
