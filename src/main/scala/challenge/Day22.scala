package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day22 extends Challenge {

  case class Instruction(op: Int, n: Option[Int])

  val pattern: Regex = "(.*) (-?\\d+)$".r
  def parse(s: String): Instruction = s match {
    case "deal into new stack"                       => Instruction(0, None)
    case pattern(a, b) if a == "deal with increment" => Instruction(1, Some(b.toInt))
    case pattern(a, b) if a == "cut"                 => Instruction(2, Some(b.toInt))
  }

  def shuffle(instructions: List[Instruction], pos: Int, size: Int): Int = {

    @tailrec
    def acc(xs: List[Instruction], x: Int): Int = xs match {
      case Instruction(0, None) :: t    => acc(t, size - x - 1)
      case Instruction(1, Some(i)) :: t => acc(t, x * i % size)
      case Instruction(2, Some(i)) :: t => acc(t, x - i % size)
      case _                            => x
    }

    acc(instructions, pos)
  }

  override def run(): Any = {
    val input = Source.fromResource("day22.txt").getLines().map(parse).toList
    shuffle(input, 2019, 10007)
  }

}
