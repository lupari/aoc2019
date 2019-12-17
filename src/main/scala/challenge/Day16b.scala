package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source


object Day16b extends Challenge {

  override def run(): Any = {
    val input: IndexedSeq[Int] =
      Source.fromResource("day16.txt").mkString.trim.toIndexedSeq.map(_.asDigit)

    val offset: Int = input.take(7).mkString.toInt
    val n = 10000 * input.length - offset
    val repeat = (n - offset % input.length - 1) / input.length + 1
    val signal = input.drop(offset % input.length) ++
      Iterator.continually(input).flatten.take(repeat * input.length).toList

    @tailrec
    def acc(xs: IndexedSeq[Int], i: Int, n: Int): IndexedSeq[Int] = i match {
      case -1 => xs
      case _ =>
        val z = math.abs(n + xs(i)) % 10
        acc(xs.updated(i, z), i - 1, z)
    }

    Iterator.iterate(signal)(acc(_, n - 1, 0)).drop(100).next.take(8).mkString.toInt
  }

}
