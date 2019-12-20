package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day20 extends Challenge {

  case class Point(x: Int, y: Int) {
    def neighbors = List(Point(x, y + 1), Point(x, y - 1), Point(x + 1, y), Point(x - 1, y))
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

  def teleport(src: Point)(implicit grid: Grid): Option[Point] = {
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

  def nextMoves(s: Point)(implicit grid: Grid): List[Point] = {
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

  def pathLength(start: Point, goal: Point)(implicit grid: Grid): Int = {
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

  def findEndpoint(label: Char)(implicit grid: Grid): Point =
    grid
      .filter(_._2 == label)
      .keys
      .filter(_.neighbors.map(grid(_)).contains(label))
      .flatMap(_.neighbors)
      .filter(grid(_) == '.')
      .head

  override def run(): Any = {
    val input               = Source.fromResource("day20.txt").mkString.toList
    implicit val grid: Grid = getGrid(input).withDefaultValue('#')

    val start = findEndpoint('A')
    val goal  = findEndpoint('Z')

    pathLength(start, goal)
  }

}
