package challenge

import base.Challenge
import lib.GridImplicits._
import lib.Points.Point

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day18 extends Challenge {

  def search(grid: Grid[Char]): Int = {
    case class State(p: Point, keys: Set[Char])
    val start    = grid.find(_._2 == '@').get
    val keyCount = grid.values.count(_.isLower)

    @tailrec
    def bfs(queue: Queue[(State, Int)], seen: Set[State]): Int = {
      val ((state, dist), q) = queue.dequeue
      if (seen.contains(state)) bfs(q, seen)
      else
        grid(state.p) match {
          case c if c == '#' || (c.isUpper && !state.keys.contains(c.toLower)) =>
            bfs(q, seen + state)
          case c if c.isLower => // found a key
            val keys = state.keys + c
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

    bfs(Queue((State(start._1, Set.empty), 0)), Set.empty)
  }

  override def run(): Any = {
    val grid = Source.fromResource("day18.txt").mkString.toList.toGrid
    search(grid)
  }

}
