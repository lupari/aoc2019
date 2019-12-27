package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day23 extends Challenge {

  case class Packet(addr: Int, x: Long, y: Long) {
    def toInput = List(x, y)
  }
  case class Computer(state: ic.Input, outBuf: List[Int], q: Queue[Packet])

  def operate(program: ic.Program): Long = {

    @tailrec
    def execute(computers: List[Computer]): Long = {
      val outcome: List[(Computer, Option[Packet])] = computers
        .map(c => (c, ic.execute(c.state)))
        .map({
          case (computer, output) =>
            val input = if (computer.q.isEmpty) List(-1L) else computer.q.head.toInput
            val queue2 = // hacky
              if (computer.q.nonEmpty && input.length == 2) computer.q.tail else computer.q
            val nextState = ic.Input(output.state, input, Some(ic.Resume(output.p, output.rb)))
            output.sig match {
              case h :: _ =>
                computer.outBuf.length match {
                  case 2 =>
                    (computer.copy(state = nextState, outBuf = Nil, q = queue2),
                     Some(Packet(computer.outBuf.head, computer.outBuf.last, h.toInt)))
                  case _ =>
                    (computer
                       .copy(state = nextState, outBuf = computer.outBuf :+ h.toInt, q = queue2),
                     None)
                }
              case _ => (computer.copy(state = nextState, q = queue2), None)
            }
        })

      val packets = outcome.flatMap(_._2)
      packets.find(_.addr == 255) match {
        case Some(p) => p.y
        case None =>
          val computers2 = outcome
            .map(_._1)
            .zipWithIndex
            .map({
              case (computer, i) =>
                packets.find(_.addr == i) match {
                  case None => computer
                  case Some(packet) =>
                    computer.copy(q = computer.q :+ packet)
                }
            })
          execute(computers2)
      }
    }

    val booted: List[Computer] = (0 until 50)
      .map(i => ic.execute(ic.Input(program, List(i), Some(ic.Resume(0, 0)))))
      .map(
        o =>
          Computer(ic.Input(o.state, Nil, Some(ic.Resume(o.p, o.rb))),
                   if (o.sig.nonEmpty) List(o.sig.head.toInt) else Nil,
                   Queue.empty))
      .toList

    execute(booted)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day23.txt"))
    operate(program)
  }

}
