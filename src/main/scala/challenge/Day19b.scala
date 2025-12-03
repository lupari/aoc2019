package challenge

import base.Challenge
import intcode.{IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day19b extends Challenge {

  def search(n: Int, lower: Int = 0, upper: Int = 10000)(check: (Int, Int) => Boolean)(
      ok: (Int, Int) => Boolean): Int = {

    @tailrec
    def bs(min: Int, max: Int): Int = {
      if (min == max - 1) min
      else {
        val mid = (min + max) / 2
        if (check(mid, n)) bs(min, mid)
        else bs(mid, max)
      }
    }

    Iterator
      .iterate((lower, upper, -1))(range => {
        val result = bs(range._1, range._2) + 1
        if (ok(result, n)) (0, 0, result)
        else {
          val mid  = (range._1 + range._2) / 2
          val next = if (!check(result, n)) (range._1, mid, -1) else (mid, range._2, -1)
          if (next._1 == next._2 - 1) (0, 0, result) else next
        }
      })
      .dropWhile(_._3 == -1)
      .next
      ._3
  }

  override def run(): Any = {
    val program               = ic.read(Source.fromResource("day19.txt"))
    def check(x: Int, y: Int) = ic.execute(ic.Input(program, List(x, y))).sig.head == 1
    def ok(x: Int, y: Int)    = check(x, y) && check(x + 99, y - 99)

    val (x, y, _) = Iterator
      .iterate((2 * 100, -1, -1))(range => {
        val x = search(range._1, lower = 2 * 100)(check)(ok)
        if (ok(x, range._1)) (x, range._1, 1) else (range._1 + 1, -1, -1)
      })
      .dropWhile(_._3 == -1)
      .next
    x * 10000 + y - 99
  }

}
