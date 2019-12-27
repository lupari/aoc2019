package challenge

import base.{Challenge, IntCode => ic}

import scala.collection.immutable.TreeMap
import scala.io.Source

object Day22 extends Challenge {

  // parse instructions => (opcode: inc, cut, new) => (inc amount, cut amount)
  // recurse xs = instructions, acc = card stack (n = 10007)
  case class Instruction(op: Int, n: Option[Int])

  val pattern = "(.*) (-?\\d+)$".r
  def parse(s: String): Instruction = s match {
    case pattern(a, _) if a == "deal into new stack" => Instruction(0, None)
    case pattern(a, b) if a == "deal with increment" => Instruction(1, Some(b.toInt))
    case pattern(a, b) if a == "cut"                 => Instruction(2, Some(b.toInt))
  }
  type Card = Int
  type Pack = TreeMap[Int, Card]

  def shuffle(instructions: List[Instruction]): List[Int] = {

    def acc(xs: List[Instruction], pack: List[Int]): List[Int] = xs match {
      case h :: t =>
        h match {
          case Instruction(0, None) => // new stack
            acc(t, pack.reverse)
          case Instruction(1, Some(i)) => // inc
            //3:  0 => 0, 1 => 3, 2 =>6 % pack size
            acc(t, pack)
          case Instruction(2, Some(i)) =>
            i match { // cut
              case n if n > 0 =>
                acc(t, pack.drop(i) ++ pack.take(i))
              case n if n < 0 =>
                acc(t, pack.takeRight(n.abs) ++ pack.take(n.abs))
            }

        }
    }

    val pack: List[Int] = (0 to 10006).toList
    acc(instructions, pack)

  }
  // 0 1 2 3 4 5 6 7 8 9
  // 0 7 4 1 8 5 2 9 6 3

  override def run(): Any = {
    val input = Source.fromResource("day22.txt").getLines().toList

  }

}
