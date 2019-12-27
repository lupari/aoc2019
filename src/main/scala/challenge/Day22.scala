package challenge

import base.Challenge

import scala.io.Source

object Day22 extends Challenge {

  case class Instruction(op: Int, n: Option[Int])

  val pattern = "(.*) (-?\\d+)$".r
  def parse(s: String): Instruction = s match {
    case "deal into new stack"                       => Instruction(0, None)
    case pattern(a, b) if a == "deal with increment" => Instruction(1, Some(b.toInt))
    case pattern(a, b) if a == "cut"                 => Instruction(2, Some(b.toInt))
  }

  def shuffle(instructions: List[Instruction], pos: Int, size: Int): Int = {

    def acc(xs: List[Instruction], p: Int): Int = xs match {
      case Instruction(0, None) :: t =>
        acc(t, size - 1 - p)
      case Instruction(1, Some(i)) :: t =>
        acc(t, ((i * p) % size) + size % size)
      case Instruction(2, Some(i)) :: t =>
        acc(t, ((p - i) % size) + size % size)
      case _ => p
    }

    acc(instructions, pos)
  }

  override def run(): Any = {
    val input = Source.fromResource("day22.txt").getLines().map(parse).toList
    shuffle(input, 2019, 10007)
  }

}
