package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day23b extends Challenge {

  case class Packet(addr: Int, x: Long, y: Long) {
    def coords = List(x, y)
  }
  case class Computer(state: ic.Input, outBuf: List[Long], q: Queue[Packet])

  def operate(program: ic.Program): Long = {

    @tailrec
    def execute(computers: Seq[Computer],
                natReg: Option[Packet] = None,
                natSent: Option[Packet] = None): Long = {
      val outcome: Seq[(Computer, Option[Packet])] = computers
        .map(c => (c, ic.execute(c.state)))
        .map({
          case (computer, output) =>
            val (in, q) =
              if (computer.q.isEmpty) (List(-1L), Queue.empty)
              else (computer.q.head.coords, computer.q.tail)
            val state = ic.Input(output.state, in, Some(ic.Resume(output.p, output.rb)))
            val c2    = computer.copy(state = state, q = q)
            output.sig match {
              case h :: _ =>
                c2.outBuf match {
                  case i :: j :: _ => (c2.copy(outBuf = Nil), Some(Packet(i.toInt, j, h)))
                  case _           => (c2.copy(outBuf = c2.outBuf :+ h), None)
                }
              case _ => (c2, None)
            }
        })

      val computers2 = outcome.map(_._1)
      (computers2.flatMap(_.outBuf), natReg) match {
        case (Nil, Some(nr)) =>
          natSent match {
            case Some(ns) if nr.y == ns.y => ns.y
            case _ => // idle network
              val computer0  = computers2.head
              val computers3 = computers2.updated(0, computer0.copy(q = computer0.q :+ nr))
              execute(computers3, natSent = Some(nr))
          }
        case _ =>
          val packets = outcome.flatMap(_._2)
          val computers3 = computers2.zipWithIndex
            .map({
              case (computer, i) => computer.copy(q = computer.q ++ packets.filter(_.addr == i))
            })
          execute(computers3, packets.find(_.addr == 255), natSent)
      }
    }

    val booted = (0 until 50)
      .map(i => ic.execute(ic.Input(program, List(i), Some(ic.Resume(0, 0)))))
      .map(o => Computer(ic.Input(o.state, Nil, Some(ic.Resume(o.p, o.rb))), Nil, Queue.empty))

    execute(booted)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day23.txt"))
    operate(program)
  }

}
