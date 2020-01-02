package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.io.Source

object Day2b extends Challenge {

  def execute(program: ic.Program): ic.Output = ic.execute(ic.Input(program, Nil))

  override def run(): Any = {
    val program                   = ic.read(Source.fromResource("day2.txt"))
    val trials: Seq[(Long, Long)] = for { x <- 0 to 99; y <- 0 to 99 } yield (x.toLong, y.toLong)
    val (x, y) = trials
      .map(t => (t, execute(program.updated(1, t._1).updated(2, t._2)).state(0)))
      .dropWhile(_._2 != 19690720)
      .head
      ._1
    100 * x + y
  }

}
