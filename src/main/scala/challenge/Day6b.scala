package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day6b extends Challenge {

  case class Orbit(item: String, centre: String)

  def parse(src: String): Orbit = {
    val parts = src.split("\\)")
    Orbit(parts.last, parts.head)
  }

  @tailrec
  def pathToCom(orbit: Orbit, xs: Map[String, Orbit], path: List[Orbit]): List[Orbit] = {
    if (xs(orbit.item).centre == "COM") path
    else pathToCom(xs(orbit.centre), xs, path :+ xs(orbit.item))
  }

  override def run(): Any = {
    val orbits: Map[String, Orbit] =
      Source.fromResource("day6.txt").getLines.map(parse).map(o => o.item -> o).toMap
    val p1 = pathToCom(orbits("YOU"), orbits, Nil)
    val p2 = pathToCom(orbits("SAN"), orbits, Nil)
    (p1 ++ p2).groupBy(identity).view.mapValues(_.size).count(_._2 == 1) - 2
  }

}
