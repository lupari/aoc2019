package challenge

import base.Challenge
import lib.GridImplicits._
import lib.Points.Point

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day20b extends Challenge {

  def teleport(src: Point)(implicit grid: Grid[Char]): Option[(Point, String)] = {
    val srcId = grid(src)
    val dstId = grid(src.neighbors.find(grid(_).isLetter).get)
    val dst = grid
      .filter(_._2 == srcId)
      .filter(_._1.neighbors.map(grid(_)).contains(dstId))
      .keys
      .toSeq
      .diff(src +: src.neighbors)
    dst match {
      case h :: _ =>
        // dst is either one of the two points that make a teleport, use the one that
        // neighbors an open point
        h.neighbors.find(grid(_) == '.') match {
          case Some(n) => Some(n, List(srcId, dstId).mkString)
          case None =>
            val next = h.neighbors.find(grid(_).isLetter).get.neighbors.find(grid(_) == '.').get
            Some((next, List(srcId, dstId).mkString))
        }
      case _ => None
    }
  }

  def portType(s: Point, x: Int, y: Int): Int = // 1 = inner, -1 = outer
    if (s.x > 3 && s.x < x - 3 && s.y > 3 && s.y < y - 3) 1
    else -1

  def canEnter(level: Int, portType: Int, label: String): Boolean = level match {
    case 0 => portType == 1 || label == "ZZ"
    case _ => label != "ZZ" && label != "AA"
  }

  def nextMoves(s: Point, level: Int)(implicit grid: Grid[Char]): List[(Point, Int)] = {
    val open = s.neighbors.filter(grid(_) == '.').map((_, 0))
    s.neighbors.find(grid(_).isLetter) match {
      case Some(port) =>
        teleport(port) match {
          case Some((tp, lbl)) =>
            val pt = portType(port, grid.keys.map(_.x).max, grid.keys.map(_.y).max)
            if (canEnter(level, pt, lbl)) open :+ (tp, pt) else open
          case None => open
        }
      case None => open
    }
  }

  def pathLength(start: Point, goal: Point, maxDepth: Int = 50)(implicit grid: Grid[Char]): Int = {
    case class State(s: Point, level: Int)

    @tailrec
    def bfs(queue: Queue[(State, Int)], seen: Set[State]): Int = {
      val ((state, dist), q) = queue.dequeue
      seen match {
        case s if s.contains(state) => bfs(q, s)
        case _ =>
          val moves = nextMoves(state.s, state.level)
          moves match {
            case m if m.map(_._1).contains(goal) && state.level == 0 => dist + 1
            case _ =>
              val states = moves
                .map(m => (State(m._1, state.level + m._2), dist + 1))
                .filter(_._1.level < maxDepth)
              bfs(q ++ states, seen + state)
          }
      }
    }

    bfs(Queue((State(start, 0), 0)), Set.empty)
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

    pathLength(start, goal, maxDepth = 26)

  }

}
