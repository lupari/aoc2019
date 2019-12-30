package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source

object Day18b extends Challenge {

  case class Point(x: Int, y: Int) {
    def neighbors = List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))
    def corners =
      List(Point(x - 1, y - 1), Point(x + 1, y - 1), Point(x - 1, y + 1), Point(x + 1, y + 1))
  }
  case class Key(dist: Int, point: Point, key: Char)

  def getGrid(input: List[Char]): (Map[Point, Char], Seq[Point]) = {

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

    val grid      = acc(input, Map.empty.withDefaultValue(' '), Point(0, 0))
    val start     = grid.find(_._2 == '@').get
    val entrances = start._1.corners.map(c => c -> '@').toMap
    val newWalls  = (start._1 +: start._1.neighbors).map(n => n -> '#').toMap
    (grid ++ entrances ++ newWalls, entrances.keys.toSeq)
  }

  def nextKeys(grid: Map[Point, Char], start: Point, keys: Set[Char]): List[Key] = {

    @tailrec
    def bfs(queue: Queue[(Point, Int)], seen: Set[Point], acc: List[Key]): List[Key] =
      queue match {
        case q if q.isEmpty => acc
        case _ =>
          val ((point, dist), q) = queue.dequeue
          if (seen.contains(point)) bfs(q, seen, acc)
          else
            grid(point) match {
              case c if c == '#' || (c.isUpper && !keys.contains(c.toLower)) =>
                bfs(q, seen + point, acc)
              case c if c.isLower && !keys.contains(c) =>
                bfs(q, seen, acc :+ Key(dist, point, c))
              case _ =>
                val states = point.neighbors.map(n => (n, dist + 1))
                bfs(q ++ states, seen + point, acc)
            }
      }

    bfs(Queue((start, 0)), Set.empty, Nil)
  }

  def search(grid: Map[Point, Char], entrances: Seq[Point], keyCount: Int): Int = {
    type VaultState = Set[(Point, Set[Char])]
    case class State(dist: Int, vaults: Seq[Point], keys: Set[Char])

    val seen: mutable.Map[Int, VaultState] =
      mutable.Map(0 -> Set(), 1 -> Set(), 2 -> Set(), 3 -> Set())
    val pq = mutable.PriorityQueue[State](State(0, entrances, Set()))(Ordering.by(-_.dist))

    def step(): Option[Int] = pq.dequeue match {
      case State(dist, _, keys) if keys.size == keyCount => Some(dist)
      case State(dist, vaults, keys) =>
        vaults.zipWithIndex.foreach({
          case (point, i) =>
            if (!seen(i).contains((point, keys))) {
              seen(i) += ((point, keys))
              pq ++= nextKeys(grid, point, keys).map(k =>
                State(dist + k.dist, vaults.updated(i, k.point), keys + k.key))
            }
        })
        None
    }

    Iterator.continually(step()).flatten.next
  }

  override def run(): Any = {
    val input             = Source.fromResource("day18.txt").mkString.toList
    val (grid, entrances) = getGrid(input)
    val keyCount          = grid.count(_._2.isLower)
    search(grid, entrances, keyCount)
  }

}
