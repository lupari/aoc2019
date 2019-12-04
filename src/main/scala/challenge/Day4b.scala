package challenge

import base.Challenge

import scala.annotation.tailrec

object Day4b extends Challenge {

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

    @tailrec
    def hasDouble(xs: Seq[Int]): Boolean = xs match {
      case h :: t =>
        t.takeWhile(_ == h).length match {
          case 1 => true
          case n => hasDouble(t.drop(n))
        }
      case _ => false
    }

    @tailrec
    def acc(pwd: Int, pwds: List[Int]): List[Int] = pwd match {
      case x if x >= end => pwds
      case x if !hasDouble(x.toString.map(_.asDigit).toList) =>
        acc(pwd + 1, pwds)
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
