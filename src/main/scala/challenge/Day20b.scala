package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day20b extends Challenge {

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

  def teleport(src: Point)(implicit grid: Grid): Option[(Point, String)] = {
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

  def portType(s: Point)(implicit g: Grid): Int = // 1 = inner, -1 = outer
    if (s.x > 3 && s.x < g.keys.maxBy(_.x).x - 3 && s.y > 3 && s.y < g.keys.maxBy(_.y).y - 3) 1
    else -1

  def canEnter(level: Int, portType: Int, label: String): Boolean = level match {
    case 0 => portType == 1 || label == "ZZ"
    case _ => label != "ZZ" && label != "AA"
  }

  def nextMoves(s: Point, level: Int)(implicit grid: Grid): List[(Point, Int)] = {
    val open = s.neighbors.filter(grid(_) == '.').map((_, 0))
    s.neighbors.find(grid(_).isLetter) match {
      case Some(port) =>
        val pt = portType(port)
        teleport(port) match {
          case Some((tp, lbl)) =>
            if (canEnter(level, pt, lbl)) open :+ (tp, pt) else open
          case None => open
        }
      case None => open
    }
  }

  def pathLength(start: Point, goal: Point, maxDepth: Int = 50)(implicit grid: Grid): Int = {
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

    pathLength(start, goal, maxDepth = 26)

  }

}
