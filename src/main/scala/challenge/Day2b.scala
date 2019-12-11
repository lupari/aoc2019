package challenge

import base.{Challenge, IntCode => ic}

import scala.io.Source

object Day2b extends Challenge {

  def execute(program: ic.Program): ic.Output = ic.execute(ic.Input(program, Nil))

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day2.txt"))
    val trials: Seq[(Long, Long)] = for { x <- 0 to 99; y <- 0 to 99 } yield (x.toLong, y.toLong)
    val (x, y) = trials.find(
      p => execute(program.updated(1, p._1).updated(2, p._2)).state(0) == 19690720
    ).get
    100 * x + y
  }

}
