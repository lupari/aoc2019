package challenge

import base.Challenge

import scala.io.Source
import scala.util.matching.Regex

object Day12 extends Challenge {

  case class ThreeD(x: Int, y: Int, z: Int) {
    def +(o: ThreeD): ThreeD = ThreeD(x + o.x, y + o.y, z + o.z)
    def sum: Int             = x.abs + y.abs + z.abs
  }
  case class Moon(pos: ThreeD, velocity: ThreeD) {
    def potential: Int = pos.sum
    def kinetic: Int   = velocity.sum
    def total: Int     = potential * kinetic
  }

  def gravity(p: ThreeD, points: List[ThreeD]): ThreeD = {
    def cmp(a: Int, b: Int): Int = a match {
      case x if x < b  => 1
      case x if x == b => 0
      case x if x > b  => -1
    }
    ThreeD(
      points.map(_.x).map(cmp(p.x, _)).sum,
      points.map(_.y).map(cmp(p.y, _)).sum,
      points.map(_.z).map(cmp(p.z, _)).sum
    )
  }

  val moonPattern: Regex = "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>".r
  def parse(moon: String): Moon = {
    val moonPattern(x, y, z) = moon
    Moon(ThreeD(x.toInt, y.toInt, z.toInt), ThreeD(0, 0, 0))
  }

  def applyMovement(moons: List[Moon]): List[Moon] = {
    moons.map(m => {
      val others = moons.diff(List(m))
      val g      = gravity(m.pos, others.map(_.pos))
      val v      = m.velocity + g
      Moon(m.pos + v, v)
    })
  }

  override def run(): Any = {
    val moons: List[Moon] = Source.fromResource("day12.txt").getLines.map(parse).toList
    (1 to 1000).foldLeft(moons)((a, _) => applyMovement(a)).map(_.total).sum
  }

}
