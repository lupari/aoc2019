package challenge

import base.Challenge
import lib.GridImplicits._
import lib.Points.Point

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day20 extends Challenge {

  def teleport(src: Point)(implicit grid: Grid[Char]): Option[Point] = {
    val srcId = grid(src)
    val dstId = grid(src.neighbors.find(grid(_).isLetter).get)
    val dst = grid
      .filter(_._2 == srcId)
      .filter(_._1.neighbors.map(grid(_)).contains(dstId))
      .keys
      .toList
      .diff(src +: src.neighbors)
    dst match {
      case h :: _ =>
        // dst is either one of the two points that make a teleport, use the one that
        // neighbors an open point
        h.neighbors.find(grid(_) == '.') match {
          case Some(n) => Some(n)
          case None =>
            val next = h.neighbors.find(grid(_).isLetter).get.neighbors.find(grid(_) == '.').get
            Some(next)
        }
      case _ => None
    }
  }

  def nextMoves(s: Point)(implicit grid: Grid[Char]): List[Point] = {
    val open = s.neighbors.filter(grid(_) == '.')
    val port = s.neighbors.find(grid(_).isLetter)
    port match {
      case Some(p) =>
        teleport(p) match {
          case Some(tp) => open :+ tp
          case None     => open
        }
      case _ => open
    }
  }

  def pathLength(start: Point, goal: Point)(implicit grid: Grid[Char]): Int = {
    case class State(s: Point)

    @tailrec
    def bfs(queue: Queue[(State, Int)], seen: Set[State]): Int = {
      val ((state, dist), q) = queue.dequeue
      seen match {
        case s if s.contains(state) => bfs(q, s)
        case _ =>
          val moves = nextMoves(state.s)
          if (moves.contains(goal)) dist + 1
          else bfs(q ++ moves.map(m => (State(m), dist + 1)), seen + state)
      }
    }

    bfs(Queue((State(start), 0)), Set.empty)
  }

  def findEndpoint(label: Char)(implicit grid: Grid[Char]): Point =
    grid
      .filter(_._2 == label)
      .keys
      .filter(_.neighbors.map(grid(_)).contains(label))
      .flatMap(_.neighbors)
      .filter(grid(_) == '.')
      .head

  override def run(): Any = {
    val input                     = Source.fromResource("day20.txt").mkString.toList
    implicit val grid: Grid[Char] = input.toGrid.withDefaultValue('#')

    val start = findEndpoint('A')
    val goal  = findEndpoint('Z')

    pathLength(start, goal)
  }

}
