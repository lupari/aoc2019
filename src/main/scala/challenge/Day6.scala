package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends Challenge {

  case class Orbit(item: String, centre: String)

  def parse(src: String): Orbit = {
    val parts = src.split("\\)")
    Orbit(parts.last, parts.head)
  }

  @tailrec
  def distanceToCom(orbit: Orbit, xs: Map[String, Orbit], dst: Int): Int = {
    if (xs(orbit.item).centre == "COM") dst + 1
    else distanceToCom(xs(orbit.centre), xs, dst + 1)
  }

  override def run(): Any = {
    val orbits: Map[String, Orbit] =
      Source.fromResource("day6.txt").getLines.map(parse).map(o => o.item -> o).toMap
    orbits.map(p => distanceToCom(p._2, orbits, 0)).sum
  }

}
