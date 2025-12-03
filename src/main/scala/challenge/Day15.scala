package challenge

import base.Challenge
import intcode.{IntCode => ic}
import lib.Graphs.AStar

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day15 extends Challenge {

  case class Square(x: Int, y: Int) {
    def neighbors: List[(Square, Int)] = List(
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

  def search(program: ic.Program): Map[Square, Int] = {

    @tailrec
    def acc(in: ic.Input, curr: Square, grid: Map[Square, Int]): Map[Square, Int] = {
      val nextSquare: Square = curr.next(in.in.head.toInt)
      ic.execute(in) match {
        case ic.Output(sig, ptr, rb, state) => // input wanted
          val newGrid = grid.updated(nextSquare, sig.head.toInt)
          sig.head.toInt match {
            case 0 => // next would hit wall
              val dir   = nextMove(newGrid, curr)
              val input = ic.Input(state, List(dir), Some(ic.State(ptr, rb)))
              acc(input, curr, newGrid)
            case 1 => // next would be ok
              val dir   = nextMove(newGrid, nextSquare)
              val input = ic.Input(state, List(dir), Some(ic.State(ptr, rb)))
              acc(input, nextSquare, newGrid)
            case 2 => // next would reach goal
              grid.updated(nextSquare, 2)
          }
      }
    }

    acc(ic.Input(program, List(1), Some(ic.State(0, 0))), Square(0, 0), Map(Square(0, 0) -> 0))
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day15.txt"))
    val graph   = search(program)
    val goal    = graph.find(_._2 == 2).get._1

    AStar(Square(0, 0), goal)((a, b) => a.x.abs - b.x.abs + a.y.abs - b.y.abs)(
      _.neighbors.map(_._1).filter(n => graph.contains(n) && graph(n) != 0))(goal)

  }

}
