package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day7b extends Challenge {

  case class Amp(in: List[Int], program: ic.Program, pointer: Long = 0)

  def amplify(amp: Amp): ic.Output =
    ic.execute(ic.Input(amp.program, amp.in, Some(ic.Resume(amp.pointer, rb = 0))))

  def feedbackLoop(amps: List[Amp]) = {

    @tailrec
    def acc(xs: List[Amp], i: Int, signals: List[Int]): Int = amplify(xs(i)) match {
      case ic.Output(_, ic.KILL, _, _) => signals.last
      case ic.Output(sig, p, _, state) =>
        val next    = (i + 1) % xs.length
        val nextSig = if (sig.isEmpty) 0 else sig.head.toInt
        val nextAmp = xs(next).copy(in = xs(next).in :+ nextSig)
        val amps    = xs.updated(i, Amp(Nil, state, p)).updated(next, nextAmp)
        acc(amps, next, signals :+ nextSig)
    }

    acc(amps, 0, Nil)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day7.txt"))
    (5 to 9).toList.permutations.zipWithIndex
      .map(
        p => p._1.map(i => Amp(if (p._2 == 0) List(i, 0) else List(i), program))
      )
      .map(feedbackLoop)
      .max
  }

}
