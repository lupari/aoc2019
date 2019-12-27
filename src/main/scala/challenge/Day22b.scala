package challenge

import base.Challenge

import scala.io.Source
import scala.util.matching.Regex

object Day22b extends Challenge {

  case class Instruction(op: Int, n: Option[Int])

  val pattern: Regex = "(.*) (-?\\d+)$".r
  def parse(s: String): Instruction = s match {
    case "deal into new stack"                       => Instruction(0, None)
    case pattern(a, b) if a == "deal with increment" => Instruction(1, Some(b.toInt))
    case pattern(a, b) if a == "cut"                 => Instruction(2, Some(b.toInt))
  }

  // helpers for preventing integer overflows
  def **(a: Long, b: Long): BigInt                  = BigInt(a) * BigInt(b)
  def %%(a: BigInt, m: Long): Long                  = a.mod(m).toLong
  def **%(a: Long, b: Long, m: Long): Long          = %%(**(a, b), m)
  def *+%(a: Long, b: Long, c: Long, m: Long): Long = ((**(a, b) + c) % m).toLong

  def inverse(instr: Instruction, a: Long, b: Long, m: Long): (Long, Long) = instr match {
    case Instruction(0, None)    => ((-a) % m, (-b - 1) % m)
    case Instruction(1, Some(n)) => (**%(a, n, m), **%(b, n, m))
    case Instruction(2, Some(n)) => (a, (b - n) % m)
    case _                       => throw new NoSuchElementException
  }

  def shuffle(xs: List[Instruction], pos: Int, n: Long, k: Long): Long = {

    def exp(a: Long, b: Long, c: Long, d: Long, e: Long) = {
      val e2 = e >> 1
      val b2 = *+%(a, b, b, n)
      val a2 = **%(a, a, n)
      val c2 = if ((e & 1) == 1) **%(a, c, n) else c
      val d2 = if ((e & 1) == 1) *+%(a, d, b, n) else d
      (a2, b2, c2, d2, e2)
    }

    val (a, b)       = xs.foldLeft(1L, 0L)((acc, instr) => inverse(instr, acc._1, acc._2, n))
    val modInvA      = BigInt(a).modInverse(n).toLong
    val (invA, invB) = (modInvA, **%(-b, modInvA, n))

    val res = Iterator
      .iterate((invA, invB, 1L, 0L, k))(p => exp(p._1, p._2, p._3, p._4, p._5))
      .dropWhile(_._5 > 0)
      .next

    %%(**(pos, res._3) + res._4, n)
  }

  override def run(): Any = {
    val input = Source.fromResource("day22.txt").getLines().map(parse).toList
    shuffle(input, 2020, 119315717514047L, 101741582076661L)
  }

}
