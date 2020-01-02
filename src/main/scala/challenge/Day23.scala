package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day23 extends Challenge {

  case class Packet(addr: Int, x: Long, y: Long) {
    def coords = List(x, y)
  }
  case class Computer(state: ic.Input, outBuf: List[Long], q: Queue[Packet])

  def operate(program: ic.Program): Long = {

    @tailrec
    def execute(computers: Seq[Computer]): Long = {
      val outcome: Seq[(Computer, Option[Packet])] = computers
        .map(c => (c, ic.execute(c.state)))
        .map({
          case (computer, output) =>
            val (in, q) =
              if (computer.q.isEmpty) (List(-1L), Queue.empty)
              else (computer.q.head.coords, computer.q.tail)
            val state = ic.Input(output.state, in, Some(ic.State(output.p, output.rb)))
            val c2    = computer.copy(state = state, q = q)
            output.sig match {
              case h :: _ =>
                computer.outBuf match {
                  case i :: j :: _ =>
                    (c2.copy(outBuf = Nil), Some(Packet(i.toInt, j, h)))
                  case _ =>
                    (c2.copy(outBuf = c2.outBuf :+ h), None)
                }
              case _ => (c2, None)
            }
        })

      val packets = outcome.flatMap(_._2)
      packets.find(_.addr == 255) match {
        case Some(packet) => packet.y
        case None =>
          val computers2 = outcome
            .map(_._1)
            .zipWithIndex
            .map({
              case (computer, i) => computer.copy(q = computer.q ++ packets.filter(_.addr == i))
            })
          execute(computers2)
      }
    }

    val booted = (0 until 50)
      .map(i => ic.execute(ic.Input(program, List(i), Some(ic.State(0, 0)))))
      .map(o => Computer(ic.Input(o.state, Nil, Some(ic.State(o.p, o.rb))), Nil, Queue.empty))

    execute(booted)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day23.txt"))
    operate(program)
  }

}
