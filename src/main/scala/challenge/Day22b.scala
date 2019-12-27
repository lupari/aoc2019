package challenge

import base.Challenge

import scala.io.Source

object Day22b extends Challenge {

  case class Instruction(op: Int, n: Option[Int])

  val pattern = "(.*) (-?\\d+)$".r
  def parse(s: String): Instruction = s match {
    case "deal into new stack"                       => Instruction(0, None)
    case pattern(a, b) if a == "deal with increment" => Instruction(1, Some(b.toInt))
    case pattern(a, b) if a == "cut"                 => Instruction(2, Some(b.toInt))
  }

  // helpers for preventing integer overflows
  def **(a: Long, b: Long): BigInt         = BigInt(a) * BigInt(b)
  def %%(a: BigInt, m: Long): Long         = a.mod(m).toLong
  def **%(a: Long, b: Long, m: Long): Long = %%(**(a, b), m)

  def inverse(instr: Instruction, a: Long, b: Long, m: Long) = instr match {
    case Instruction(0, None)    => ((-a) % m, (-b - 1) % m)
    case Instruction(1, Some(n)) => (**%(a, n, m), **%(b, n, m))
    case Instruction(2, Some(n)) => (a, (b - n) % m)
    case _                       => throw new NoSuchElementException
  }

  def solve(instr: List[Instruction], pos: Int, size: Long, repeats: Long) = {
    val m = size

    val (a, b) =
      instr.foldLeft(1L, 0L)((inp, instr) => inverse(instr, inp._1, inp._2, m))
    val modInvA      = BigInt(a).modInverse(m).toLong
    val (invA, invB) = (modInvA, **%(-b, modInvA, m))

    def acc(a: Long, b: Long, c: Long, d: Long, e: Long) = {
      val e2 = e >> 1
      val b2 = %%(**(a, b) + b, m)
      val a2 = **%(a, a, m)
      val c2 = if ((e & 1) == 1) **%(a, c, m) else c
      val d2 = if ((e & 1) == 1) %%(**(a, d) + b, m) else d
      (a2, b2, c2, d2, e2)
    }

    val res = Iterator
      .iterate((invA, invB, 1L, 0L, repeats.abs))(p => acc(p._1, p._2, p._3, p._4, p._5))
      .dropWhile(_._5 > 0)
      .next

    %%(**(pos, res._3) + res._4, m)
  }

  override def run(): Any = {
    val input = Source.fromResource("day22.txt").getLines().map(parse).toList
    solve(input, 2020, 119315717514047L, -101741582076661L)
  }

}
