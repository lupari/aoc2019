package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

  def search(grid: Map[Point, Char]) = {
    case class State(p: Point, keys: Set[Char])
    val start    = grid.find(_._2 == '@').get
    val keyCount = grid.values.count(v => v.isLetter && v.isLower)

    @tailrec
    def bfs(queue: Queue[(State, Int)], seen: Set[State]): Int = {
      val ((state, dist), q) = queue.dequeue
      if (seen.contains(state)) bfs(q, seen)
      else if (grid(state.p) == '#') bfs(q, seen + state)
      else
        grid(state.p) match {
          case c if c.isLetter && c.isUpper && !state.keys.contains(c.toLower) =>
            bfs(q, seen + state) // door, no key
          case c =>
            c match {
              case k if k.isLetter && k.isLower => // found a key
                val keys = state.keys + k
                keys.size match {
                  case kc if kc == keyCount => dist
                  case _ =>
                    val states = state.p.neighbors.map(n => (State(n, keys), dist + 1))
                    bfs(q ++ states, seen + state)
                }
              case _ =>
                val states = state.p.neighbors.map(n => (State(n, state.keys), dist + 1))
                bfs(q ++ states, seen + state)
            }
        }
    }

    bfs(Queue((State(start._1, Set.empty), 0)), Set.empty)

  }

  override def run(): Any = {
    val input = Source.fromResource("day18.txt").mkString.toList
    val grid  = getGrid(input)
    search(grid)
  }

}
