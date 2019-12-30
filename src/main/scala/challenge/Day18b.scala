package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source

object Day18b extends Challenge {

  case class Point(x: Int, y: Int) {
    def neighbors = List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))
    def diags =
      List(Point(x - 1, y - 1), Point(x + 1, y - 1), Point(x - 1, y + 1), Point(x + 1, y + 1))
  }

  def getGrid(input: List[Char]): (Map[Point, Char], Point) = {

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

    val grid                        = acc(input, Map.empty.withDefaultValue(' '), Point(0, 0))
    val start                       = grid.find(_._2 == '@').get
    val newStarts: Map[Point, Char] = start._1.diags.map(d => (d -> '@')).toMap
    val newWalls: Map[Point, Char]  = start._1.neighbors.map(n => (n -> '#')).toMap
    ((grid ++ newStarts ++ newWalls).updated(start._1, '#'), start._1)
  }

  def search(grid: Map[Point, Char], start: Point, keys: Set[Char]) = {
    case class State(p: Point)
    case class KeyFoundEvent(dist: Int, point: Point, key: Char)

    // THIS SHOULD BE AN ITERATOR OR SMTH LIKE THAT

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
                case k if k.isLetter && k.isLower => // found a key
                  val states = state.p.neighbors.map(n => (State(n), dist + 1))
                  bfs(q ++ states, seen + state, events :+ KeyFoundEvent(dist, state.p, k))
                case _ =>
                  val states = state.p.neighbors.map(n => (State(n), dist + 1))
                  bfs(q ++ states, seen + state, events)
              }
          }
      }
    }

    bfs(Queue((State(start), 0)), Set.empty, Nil)

  }

  override def run(): Any = {
    val input         = Source.fromResource("day18.txt").mkString.toList
    val (grid, start) = getGrid(input)
    val (x, y)        = (grid.keys.maxBy(_.x).x, grid.keys.maxBy(_.y).y)
    val canvas        = Array.tabulate(y + 1, x + 1)((_, _) => ' ')
    for { p <- grid } yield canvas(p._1.y)(p._1.x) = p._2
    canvas foreach { row =>
      row.mkString foreach print; println
    }
    val starts  = start.diags
    val allKeys = grid.filter(_._2.isLower).map(_._2).toSet

    case class GlobalState(dist: Int, vaults: List[Point], keys: Set[Char])
    val seen: mutable.Map[Int, Set[(Point, Set[Char])]] =
      mutable.Map(0 -> Set(), 1 -> Set(), 2 -> Set(), 3 -> Set())

    val pq = mutable.PriorityQueue[GlobalState]()(Ordering.by((gs: GlobalState) => -gs.dist))
    pq.enqueue(GlobalState(0, starts, Set.empty))
    while (pq.nonEmpty && pq.head.keys != allKeys) {
      val gs = pq.dequeue()
      if (gs.keys == allKeys) {
        println(gs.dist)
      } else {
        gs.vaults.zipWithIndex.foreach({
          case (point, i) if !seen(i).contains((point, gs.keys)) =>
            seen + (i -> (seen(i) + ((point, gs.keys))))
            val events = search(grid, point, gs.keys)
            events.foreach(e => {
              //println("key found: " + (i, e))
              pq.enqueue(
                GlobalState(gs.dist + e.dist, gs.vaults.updated(i, e.point), gs.keys + e.key))
            })
          //println("done with " + i)
        })
      }
    }

    println("ready now?")

    // Setup a registry of seen places for each vault (Set(), Set(), Set(), Set())
    // Setup a priority queue of (dist -> (points, keys))
    // initially: (0 -> starts, empty set)
    // while queue not empty
    // dequeue : d -> (points, keys)
    // if (keys == allkeys) yield d
    // for all points.zipWithIndex i, p
    // if (p, keys) in seen(i) continue
    // else seen(i) :+ (p, keys)
    // for d2, p2, key in search(p, keys)
    // q.push(d + d2 -> points(i) = p2, keys :+ key)

  }

}
