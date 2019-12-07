package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day7b extends Challenge {

  case class Output(sig: Int, p: Int, state: List[Int])
  case class Amp(in: List[Int], program: List[Int], pointer: Int)
  val KILL = -1

  def amplify(amp: Amp): Output = {

    def parse(d: Int): (Int, Int, Int, Int) =
      (d % 100, d / 100 % 10, d / 1000 % 10, d / 10000 % 10)

    def value(xs: List[Int], index: Int, mode: Int): Int =
      if (mode == 0) xs(xs(index)) else xs(index)

    @tailrec
    def acc(p: Int, xs: List[Int], in: List[Int], out: Int): Output = xs(p) match {
      case 99 => Output(out, KILL, xs)
      case 3 => in match {
        case h :: t => acc(p + 2, xs.updated(xs(p + 1), h), t, out)
        case _ => Output(out, p, xs) // halt as no input available
      }
      case instr =>
        val (opcode, m1, m2, _) = parse(instr)
        val v1                  = value(xs, p + 1, m1)
        opcode match {
          case 1 => // sum
            val v2 = value(xs, p + 2, m2)
            acc(p + 4, xs.updated(xs(p + 3), v1 + v2), in, out)
          case 2 => // mul
            val v2 = value(xs, p + 2, m2)
            acc(p + 4, xs.updated(xs(p + 3), v1 * v2), in, out)
          case 4 => Output(v1, p + 2, xs) // send signal and halt
          case 5 => // jmp-true
            val v2 = value(xs, p + 2, m2)
            acc(if (v1 != 0) v2 else p + 3, xs, in, out)
          case 6 => // jmp-false
            val v2 = value(xs, p + 2, m2)
            acc(if (v1 == 0) v2 else p + 3, xs, in, out)
          case 7 => // eq
            val v2 = value(xs, p + 2, m2)
            acc(p + 4, xs.updated(xs(p + 3), if (v1 < v2) 1 else 0), in, out)
          case 8 => // lt
            val v2 = value(xs, p + 2, m2)
            acc(p + 4, xs.updated(xs(p + 3), if (v1 == v2) 1 else 0), in, out)
        }
    }

    acc(amp.pointer, amp.program, amp.in, 0)
  }

  def feedbackLoop(amps: List[Amp]) = {

    @tailrec
    def acc(xs: List[Amp], i: Int, signals: List[Int]): Int = amplify(xs(i)) match {
      case Output(_, KILL, _) => signals.last
      case Output(sig, p, state) =>
        val next = (i + 1) % xs.length
        val nextAmp = xs(next).copy(in = xs(next).in :+ sig)
        val amps = xs.updated(i, Amp(Nil, state, p)).updated(next, nextAmp)
        acc(amps, next, signals :+ sig)
    }

    acc(amps, 0, Nil)
  }

  override def run(): Any = {
    val program = Source.fromResource("day7.txt").mkString.split(",").map(_.trim.toInt).toList
    (5 to 9).toList.permutations.zipWithIndex.map(
      p => p._1.map(i => Amp(if (p._2 == 0) List(i, 0) else List(i), program, 0))
    ).map(feedbackLoop).max
  }

}
