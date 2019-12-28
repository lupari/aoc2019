package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Ordered.orderingToOrdered

object Day24 extends Challenge {

  case class Point(x: Int, y: Int) extends Ordered[Point] {
    override def compare(p: Point): Int = (y, x).compareTo((p.y, p.x))
    def neighbors: List[Point] =
      List(Point(x, y + 1), Point(x, y - 1), Point(x + 1, y), Point(x - 1, y))
  }
  type Grid = Map[Point, Char]

  def getGrid(input: List[Char]): Grid = {

    @tailrec
    def acc(xs: List[Char], grid: Grid, current: Point): Grid =
      xs match {
        case h :: t =>
          h.toChar match {
            case '\n' =>
              acc(t, grid, Point(0, current.y + 1))
            case c =>
              acc(t, grid.updated(current, c), Point(current.x + 1, current.y))
          }
        case _ => grid
      }

    acc(input, Map.empty, Point(0, 0))
  }

  def calculate(grid: Grid): Long = {

    def nextState(g: Grid, p: Point, c: Char): Char = {
      val bugCount = p.neighbors.map(g(_)).count(_ == '#')
      c match {
        case '#' =>
          if (bugCount == 1) '#' else '.'
        case '.' =>
          if (bugCount == 1 || bugCount == 2) '#' else '.'
      }
    }

    def transform(g: Grid): Grid =
      g.map({ case (k, v) => (k, nextState(g, k, v)) }).withDefaultValue('-')

    def bdr(g: Grid): Long = {
      val bugs = g.keys.toList.sorted.map(g(_)).zipWithIndex.filter(_._1 == '#').map(_._2)
      bugs.map(math.pow(2, _).toLong).sum
    }

    Iterator
      .iterate((List.empty[Long], grid))({
        case (ratings, g) =>
          val next = transform(g)
          (ratings :+ bdr(next), next)
      })
      .dropWhile({ case (ratings, _) => ratings.distinct.size == ratings.size })
      .next
      ._1
      .last
  }

  override def run(): Any = {
    val input = Source.fromResource("day24.txt").mkString.toList
    val grid  = getGrid(input).withDefaultValue('-')
    calculate(grid)
  }
}
