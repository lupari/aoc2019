package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends Challenge {

  def execute(program: Map[Long, Long], inputs: List[Int]): List[Long] = {

    def parse(d: Int): (Int, Int, Int, Int) =
      (d % 100, d / 100 % 10, d / 1000 % 10, d / 10000 % 10)

    def value(xs: Map[Long, Long], index: Long, rb: Long, mode: Int) = mode match {
      case 0 => xs(xs(index))
      case 1 => xs(index)
      case 2 => xs(rb + xs(index)) // xs(rb + xs(index))
    }

    @tailrec
    def acc(p: Long, xs: Map[Long, Long], in: List[Int], rb: Long, out: List[Long]): List[Long] =
      xs(p) match {
        case 99 => out
        case instr =>
          val (opcode, m1, m2, m3) = parse(instr.toInt)
          val v1                   = value(xs, p + 1, rb, m1)
          val v2                   = value(xs, p + 2, rb, m2)
          val v3                   = if (m3 == 2) rb + xs(p + 3) else xs(p + 3)
          opcode match {
            case 1 => // sum
              acc(p + 4, xs.updated(v3, v1 + v2), in, rb, out)
            case 2 => // mul
              acc(p + 4, xs.updated(v3, v1 * v2), in, rb, out)
            case 3 => // read
              val i = if (m1 == 2) rb + xs(p + 1) else xs(p + 1)
              acc(p + 2, xs.updated(i, in.head), in.drop(1), rb, out)
            case 4 => // write
              acc(p + 2, xs, in, rb, out :+ v1)
            case 5 => // jmp-true
              acc(if (v1 != 0) v2 else p + 3, xs, in, rb, out)
            case 6 => // jmp-false
              acc(if (v1 == 0) v2 else p + 3, xs, in, rb, out)
            case 7 => // lt
              acc(p + 4, xs.updated(v3, if (v1 < v2) 1 else 0), in, rb, out)
            case 8 => // eq
              acc(p + 4, xs.updated(v3, if (v1 == v2) 1 else 0), in, rb, out)
            case 9 => // rb
              acc(p + 2, xs, in, rb + v1, out)
          }
      }

    acc(0, program, inputs, 0, Nil)
  }

  override def run(): Any = {
    val input = Source.fromResource("day9.txt").mkString.split(",").map(_.trim.toLong).toList
    val program: Map[Long, Long] =
      input.zipWithIndex.map(x => x._2.toLong -> x._1).toMap.withDefaultValue(0)
    execute(program, List(1)).head
  }

}
