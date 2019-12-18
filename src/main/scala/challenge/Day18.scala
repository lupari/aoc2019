package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day18 extends Challenge {

  case class Point(x: Int, y: Int) {
    def neighbors = List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))
  }

  def getGrid(input: List[Char]): Map[Point, Char] = {

    @tailrec
    def acc(xs: List[Char], grid: Map[Point, Char], current: Point): Map[Point, Char] = xs match {
      case h :: t =>
        h.toChar match {
          case '\n' =>
            acc(t, grid, Point(0, current.y + 1))
          case c =>
            acc(t, grid.updated(current, c), Point(current.x + 1, current.y))
        }
      case _ => grid
    }

    acc(input, Map.empty.withDefaultValue(' '), Point(0, 0))
  }

  object AStar {
    def apply[A](start: A, goal: A)(cf: (A, A) => Int)(nf: A => List[A]): Map[A, Int] = {
      val open  = mutable.ListBuffer[(A, Int)]((start, 0))
      val costs = mutable.Map[A, Int](start -> 0)
      while (open.nonEmpty) {
        val current = open.minBy(_._2)
        open -= current
        if (current._1 == goal) open.clear
        else {
          val newCost   = costs(current._1) + 1
          val neighbors = nf(current._1)
          for (next <- neighbors) {
            if (!costs.contains(next) || newCost < costs(next)) {
              costs(next) = newCost
              val priority = newCost + cf(goal, next)
              open += ((next, priority))
            }
          }
        }
      }
      costs.toMap.withDefaultValue(Int.MaxValue)
    }
  }

  def isBlocked(c: Char)                 = c == '#' || (c.isLetter && c.isUpper)
  def isDoorBlocked(c: Char, door: Char) = c == '#' || (c.isLetter && c.isUpper && c != door)
  def costFn(a: Point, b: Point): Int    = a.x.abs - b.x.abs + a.y.abs - b.y.abs
  def adjFn(a: Point, g: Map[Point, Char]): List[Point] =
    a.neighbors.filter(n => g.contains(n) && !isBlocked(g(n)))

  case class State(p: Point, keys: Set[Char], dist: Int)
  def bfs(grid: Map[Point, Char]) = {
    val start                       = grid.find(_._2 == '@').get
    val allKeys: Set[Char]          = grid.values.filter(v => v.isLetter && v.isLower).toSet
    val queue: mutable.Queue[State] = mutable.Queue(State(start._1, Set.empty, 0))

    @tailrec
    def acc(seen: Set[(Point, Set[Char])]): Int = {
      val state     = queue.dequeue()
      val stateRepr = (state.p, state.keys)
      if (seen.contains(stateRepr)) acc(seen)
      else if (grid(state.p) == '#') acc(seen + stateRepr)
      else
        grid(state.p) match {
          case c if c.isLetter && c.isUpper && !state.keys.contains(c.toLower) =>
            acc(seen + stateRepr) // door, no key
          case c =>
            val neighbors = state.p.neighbors.filter(grid.contains(_))
            c match {
              case k if k.isLetter && k.isLower => // found a key
                val keys = state.keys + k
                if (keys == allKeys) state.dist
                else {
                  val states = neighbors.map(n => State(n, keys, state.dist + 1))
                  states.foreach(queue.enqueue(_))
                  acc(seen + stateRepr)
                }
              case _ =>
                val states = neighbors.map(n => State(n, state.keys, state.dist + 1))
                states.foreach(queue.enqueue(_))
                acc(seen + stateRepr)
            }
        }
    }

    acc(Set.empty)

  }

  override def run(): Any = {
    val input = Source.fromResource("day18.txt").mkString.toList
    val grid  = getGrid(input)
    bfs(grid)
  }

}
