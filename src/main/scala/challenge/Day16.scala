package challenge

import base.Challenge

import scala.io.Source

object Day16 extends Challenge {

  def pattern(base: List[Int], ord: Int): IndexedSeq[Int] =
    base.view.flatMap(Iterator.fill(ord)(_)).toIndexedSeq

  def fft(xs: IndexedSeq[Int], base: List[Int]): IndexedSeq[Int] =
    (1 to xs.length)
      .map(i => {
        val p = pattern(base, i)
        xs.zipWithIndex.map(x => x._1 * p((x._2 + 1) % p.length)).sum.abs % 10
      })

  override def run(): Any = {
    val input: IndexedSeq[Int] =
      Source.fromResource("day16.txt").mkString.trim.toIndexedSeq.map(_.asDigit)
    Iterator.iterate(input)(fft(_, List(0, 1, 0, -1))).drop(100).next().take(8).mkString.toInt
  }

}
