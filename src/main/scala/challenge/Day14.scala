package challenge

import base.Challenge

import scala.io.Source
import scala.util.matching.Regex


object Day14 extends Challenge {

  type Chemical = String

  val reactionPattern: Regex = ".* => (\\d+) (.+)$".r
  def parse(src: String): (Chemical, Int) = {
    val reactionPattern(amount, chemical) = src
    (chemical, amount.toInt)
  }

  def children(src: String): List[(Int, Chemical)] =
    src.split(" => ").head.split(", ").flatMap(_.split(" ")).grouped(2).map(x => (x.head.toInt, x.last)).toList

  def amount(reactions: List[String]): Int = {

    def acc(chemical: Chemical, called: Int): Int = {
      val src: String = reactions.find(r => parse(r)._1 == chemical).get
      val (_, count) = parse(src)
      val needed: List[(Int, String)] = children(src)
      println(count + " of chemical " + chemical + " needs " + needed)
      if (needed.head._2 == "ORE") {
        println("ret: " + needed.head._1)
        count / needed.head._1
      } else {
        val am = needed.map(n => n._1 * acc(n._2, count)).sum
        println(" ret rec: needed " +  needed + " got " + am)
        am
      }
    }

    acc("FUEL", 1)
  }


  override def run(): Any = {
    val input: List[String] = List(
      "2 ORE => 2 A",
      "1 ORE => 1 B",
      "7 A, 1 B => 1 C",
      "7 A, 1 C => 1 D",
      "7 A, 1 D => 1 E",
      "7 A, 1 E => 1 FUEL")
    amount(input)
  }

  // 7A()

}
