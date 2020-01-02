package challenge

import base.Challenge
import lib.GridImplicits._
import lib.Points.Point

import scala.io.Source

object Day24 extends Challenge {

  def calculate(grid: Grid[Char]): Long = {

    def nextState(g: Grid[Char], p: Point, c: Char): Char = {
      val bugCount = p.neighbors.map(g(_)).count(_ == '#')
      c match {
        case '#' =>
          if (bugCount == 1) '#' else '.'
        case '.' =>
          if (bugCount == 1 || bugCount == 2) '#' else '.'
      }
    }

    def transform(g: Grid[Char]): Grid[Char] =
      g.map({ case (k, v) => (k, nextState(g, k, v)) }).withDefaultValue('-')

    def bdr(g: Grid[Char]): Long = {
      val bugs =
        g.keys.toList
          .sortBy({ case Point(x, y) => (y, x) })
          .map(g(_))
          .zipWithIndex
          .filter(_._1 == '#')
          .map(_._2)
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
    val grid  = input.toGrid.withDefaultValue('-')
    calculate(grid)
  }
}
