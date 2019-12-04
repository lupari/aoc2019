package challenge

import base.Challenge

object Day4 extends Challenge {

  def candidates(start: Int, end: Int): List[Int] = {

    def checkOrder(i: Int): (Boolean, Int) = {
      val pwd = i.toString.map(_.asDigit)
      pwd.zipWithIndex.sliding(2).find(p => p.head._1 > p.last._1) match {
        case Some(p) =>
          val next: Int = pwd.patch(p.last._2, Seq(pwd(p.last._2) + 1), 1).mkString.toInt
          (false, next)
        case _ => (true, i + 1)
      }
    }

    def hasDouble(i: Int): Boolean =
      i.toString.map(_.asDigit).sliding(2).exists(x => x.head == x.last)

    def acc(pwd: Int, pwds: List[Int]): List[Int] = pwd match {
      case x if x >= end      => pwds
      case x if !hasDouble(x) => acc(pwd + 1, pwds)
      case _ =>
        val result = checkOrder(pwd)
        acc(result._2, if (result._1) pwds :+ pwd else pwds)
    }

    acc(start, List())
  }

  override def run(): Any = {
    candidates(156218, 652527).length
  }

}
