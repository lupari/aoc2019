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
    val newStarts = start._1.corners.map(c => c -> '@').toMap
    val newWalls  = (start._1 +: start._1.neighbors).map(n => n -> '#').toMap
    (grid ++ newStarts ++ newWalls, newStarts.keys.toSeq)
  }

  case class KeyFoundEvent(dist: Int, point: Point, key: Char)
  def search(grid: Map[Point, Char], start: Point, keys: Set[Char]): List[KeyFoundEvent] = {
    case class State(p: Point)

    @tailrec
    def bfs(queue: Queue[(State, Int)],
            seen: Set[State],
            events: List[KeyFoundEvent]): List[KeyFoundEvent] = {
      if (queue.isEmpty) events
      else {
        val ((state, dist), q) = queue.dequeue
        if (seen.contains(state)) bfs(q, seen, events)
        else if (grid(state.p) == '#') bfs(q, seen + state, events)
        else
          grid(state.p) match {
            case c if c.isUpper && !keys.contains(c.toLower) =>
              bfs(q, seen + state, events) // door, no key
            case c =>
              c match {
                case k if k.isLetter && k.isLower && !keys.contains(k) => // found a new key
                  bfs(q, seen, events :+ KeyFoundEvent(dist, state.p, k))
                case _ =>
                  val states = state.p.neighbors.map(n => (State(n), dist + 1))
                  bfs(q ++ states, seen + state, events)
              }
          }
      }
    }

    bfs(Queue((State(start), 0)), Set.empty, Nil)

  }

  def searchVaults(grid: Map[Point, Char], entrances: Seq[Point], keys: Set[Char]): Int = {
    case class GlobalState(dist: Int, vaults: Seq[Point], keys: Set[Char])

    val seen: mutable.Map[Int, Set[(Point, Set[Char])]] =
      mutable.Map(0 -> Set(), 1 -> Set(), 2 -> Set(), 3 -> Set())

    val pq = mutable.PriorityQueue[GlobalState]()(Ordering.by((gs: GlobalState) => -gs.dist))
    pq.enqueue(GlobalState(0, entrances, Set.empty))

    def step(): Option[Int] = {
      val gs = pq.dequeue()
      gs.keys match {
        case gk if gk.size == keys.size => Some(gs.dist)
        case _ =>
          gs.vaults.zipWithIndex.foreach({
            case (point, i) =>
              if (!seen(i).contains((point, gs.keys))) {
                seen(i) = seen(i) + ((point, gs.keys))
                val events = search(grid, point, gs.keys)
                events.foreach(e => {
                  pq.enqueue(
                    GlobalState(gs.dist + e.dist, gs.vaults.updated(i, e.point), gs.keys + e.key))
                })
              }
          })
          None
      }
    }

    Iterator.continually(step()).dropWhile(_.isEmpty).next.get
  }

  override def run(): Any = {
    val input          = Source.fromResource("day18.txt").mkString.toList
    val (grid, starts) = getGrid(input)
    val allKeys        = grid.filter(_._2.isLower).values.toSet
    searchVaults(grid, starts, allKeys)
  }

}
