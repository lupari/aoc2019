package challenge

import base.Challenge
import lib.GridImplicits._
import lib.Points.Point

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source

object Day18b extends Challenge {

  case class Key(dist: Int, point: Point, key: Char)

  def nextKeys(grid: Grid[Char], start: Point, keys: Set[Char]): List[Key] = {

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
                val states = point.neighbors.map((_, dist + 1))
                bfs(q ++ states, seen + point, acc)
            }
      }

    bfs(Queue((start, 0)), Set.empty, Nil)
  }

  def search(grid: Grid[Char], entrances: Seq[Point], keyCount: Int): Int = {
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
    val grid0             = Source.fromResource("day18.txt").mkString.toList.toGrid
    val keyCount          = grid0.count(_._2.isLower)
    val start             = grid0.find(_._2 == '@').get
    val corners           = start._1.corners.map(c => c -> '@').toMap
    val newWalls          = (start._1 +: start._1.neighbors).map(n => n -> '#').toMap
    val (grid, entrances) = (grid0 ++ corners ++ newWalls, corners.keys.toSeq)
    search(grid, entrances, keyCount)
  }

}
