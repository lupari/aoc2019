package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day15b extends Challenge {

  case class Square(x: Int, y: Int) {
    def neighbors(): List[(Square, Int)] = List(
      (Square(x, y + 1), 1),
      (Square(x, y - 1), 2),
      (Square(x + 1, y), 4),
      (Square(x - 1, y), 3)
    )
    def next(c: Int): Square = neighbors.find(_._2 == c).get._1
  }

  def nextMove(grid: Map[Square, Int], s: Square): Int = {
    val neighbors: List[(Square, Int)] =
      s.neighbors.filterNot(n => grid.contains(n._1) && grid(n._1) == 0)
    val notFound: List[(Square, Int)] = neighbors.filterNot(n => grid.contains(n._1))
    val found: List[(Square, Int)]    = neighbors.filter(n => grid.contains(n._1))
    if (notFound.nonEmpty) notFound(Random.nextInt(notFound.length))._2
    else found(Random.nextInt(found.length))._2
  }

  def search(program: ic.Program, bound: Int): Map[Square, Int] = {

    @tailrec
    def acc(in: ic.Input,
            curr: Square,
            grid: Map[Square, Int],
            t: Long,
            goalFound: Boolean = false): Map[Square, Int] = {
      if (t > bound && goalFound) grid
      else {
        val nextSquare: Square = curr.next(in.in.head)
        ic.execute(in) match {
          case ic.Output(sig, ptr, rb, state) => // input wanted
            val newGrid = grid.updated(nextSquare, sig.head.toInt)
            sig.head.toInt match {
              case 0 => // next would hit wall
                val dir   = nextMove(newGrid, curr)
                val input = ic.Input(state, List(dir), Some(ic.Resume(ptr, rb)))
                acc(input, curr, newGrid, t + 1, goalFound)
              case _ => // next would be ok
                val dir   = nextMove(newGrid, nextSquare)
                val input = ic.Input(state, List(dir), Some(ic.Resume(ptr, rb)))
                acc(input, nextSquare, newGrid, t + 1, if (sig.head.toInt == 2) true else goalFound)
            }
        }
      }
    }

    acc(ic.Input(program, List(1), Some(ic.Resume(0, 0))), Square(0, 0), Map(Square(0, 0) -> 0), 0)
  }

  def oxygenationTime(maze: Map[Square, Int]): Int = {

    @tailrec
    def acc(xs: Map[Square, Int], t: Int): Int = {
      val oxygenated = xs.filter(_._2 == 4)
      val neighbors: Set[Square] = oxygenated
        .flatMap(_._1.neighbors())
        .keys
        .filter(k => xs.contains(k) && xs(k) == 1)
        .toSet
      if (neighbors.isEmpty) t
      else acc(xs ++ neighbors.map(_ -> 4), t + 1)
    }

    acc(maze.updated(maze.find(_._2 == 2).get._1, 4), 0)

  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day15.txt"))
    // TODO: find a more elegant way (BFS?) to fully search the graph
    // now lets just assume that 100K movements should do it
    val maze = search(program, 100000)
    oxygenationTime(maze)
  }

}
