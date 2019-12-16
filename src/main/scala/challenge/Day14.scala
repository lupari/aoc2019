package challenge

import base.Challenge

import scala.io.Source

object Day14 extends Challenge {

  type Reactions = Map[String, (Int, Map[String, Int])]

  def oreCount(reactions: Reactions): Int = {

    def acc(chemical: String, n: Int, surplus: Map[String, Int]): (Int, Map[String, Int]) =
      chemical match {
        case "ORE" => (n, surplus)
        case _ =>
          val needed                   = math.max(0, n - surplus(chemical))
          val (produced, requirements) = reactions(chemical)
          // figure out if we need an extra reaction for gathering all required stuff
          val (q, m)               = (needed / produced, needed % produced)
          val (repeats, leftovers) = if (m == 0) (q, m) else (q + 1, produced - m)

          val surplusAmount = surplus(chemical) - n + needed
          val (ore, surplusProduced) =
            requirements.foldLeft((0, surplus.updated(chemical, surplusAmount)))((a, b) => {
              val (o2, s2) = acc(b._1, repeats * b._2, a._2)
              (a._1 + o2, s2)
            })

          val newSurplusAmount = surplusProduced(chemical) + leftovers
          (ore, surplusProduced.updated(chemical, newSurplusAmount))
      }

    acc("FUEL", 1, Map().withDefaultValue(0))._1
  }

  def parseReactions(src: List[String]): Reactions = {
    src
      .map(s => {
        val req = "\\d+ [A-Z]+".r
          .findAllMatchIn(s)
          .toList
          .map(s => s.matched.split(" "))
          .map(l => (l.last, l.head.toInt))
        req.last._1 -> (req.last._2, req.dropRight(1))
      })
      .map(s => s._1 -> (s._2._1 -> s._2._2.toMap))
      .toMap
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day14.txt").getLines.toList
    val reactions           = parseReactions(input)
    oreCount(reactions)
  }

}
