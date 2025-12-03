package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Day12b extends Challenge {

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

  def applyMovement(moons: List[Moon]): List[Moon] =
    moons.map(m => {
      val others = moons.diff(List(m))
      val g      = gravity(m.pos, others.map(_.pos))
      val v      = m.velocity + g
      Moon(m.pos + v, v)
    })

  def cycleLength[A, B](xs: List[A])(f: List[A] => List[A])(g: A => B): Int = {
    val memo: mutable.Map[List[B], List[B]] = mutable.Map()
    Iterator
      .iterate((xs, Nil))(p => (f(p._1), p._2))
      .zipWithIndex
      .map(x => (x._2, memo.put(x._1._1.map(g), x._1._2.map(g))))
      .dropWhile(x => x._2.isEmpty)
      .next
      ._1
  }

  def stepsUntilIdentical(moons: List[Moon]): Long = {
    val f   = cycleLength(moons)(applyMovement) _
    val clx = f(m => (m.pos.x, m.velocity.x))
    val cly = f(m => (m.pos.y, m.velocity.y))
    val clz = f(m => (m.pos.z, m.velocity.z))

    @tailrec
    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
    def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

    lcm(lcm(clx, cly), clz)
  }

  override def run(): Any = {
    val moons: List[Moon] = Source.fromResource("day12.txt").getLines.map(parse).toList
    stepsUntilIdentical(moons)
  }

}
