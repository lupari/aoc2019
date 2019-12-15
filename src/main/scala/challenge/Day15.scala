package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable
import scala.util.Random

object Day15 extends Challenge {

  case class Square(x: Int, y: Int) {
    def neighbors(): List[(Square, Int)] = List(
      (Square(x, y + 1), 1), (Square(x, y - 1), 2), (Square(x + 1, y), 4), (Square(x - 1, y), 3)
    )
    def next(c: Int): Square = neighbors.find(_._2 == c).get._1
  }

  def nextMove(grid: Map[Square, Int], s: Square): Int = {
    val neighbors: List[(Square, Int)] = s.neighbors.filterNot(n => grid.contains(n._1) && grid(n._1) == 0)
    val notFound: List[(Square, Int)] = neighbors.filterNot(n => grid.contains(n._1))
    val found: List[(Square, Int)] = neighbors.filter(n => grid.contains(n._1))
    if (notFound.nonEmpty) notFound(Random.nextInt(notFound.length))._2 else found(Random.nextInt(found.length))._2
  }

  def play(program: ic.Program): Map[Square, Int] = {

    @tailrec
    def acc(in: ic.Input, curr: Square, grid: Map[Square, Int]): Map[Square, Int] = {
      val nextSquare: Square = curr.next(in.in.head)
      ic.execute(in) match {
        case ic.Output(sig, ptr, rb, state) => // input wanted
          val newGrid = grid.updated(nextSquare, sig.head.toInt)
          sig.head.toInt match {
            case 0 => // next would hit wall
              val dir = nextMove(newGrid, curr)
              val input = ic.Input(state, List(dir), Some(ic.Resume(ptr, rb)))
              acc(input, curr, newGrid)
            case 1 => // next would be ok
              val dir = nextMove(newGrid, nextSquare)
              val input = ic.Input(state, List(dir), Some(ic.Resume(ptr, rb)))
              acc(input, nextSquare, newGrid)
            case 2 => // next would reach goal
              println("2 found in " + nextSquare)
              grid.updated(nextSquare, 2)
          }
      }
    }

    // syötä input, lue output ja tila jne

    acc(ic.Input(program, List(1), Some(ic.Resume(0, 0))), Square(0, 0), Map(Square(0, 0) -> 0))
  }

  def astar[A](start: A, goal: A)(cf: (A, A) => Int)(af: A => List[A]): Map[A, Int] = {
   val open = mutable.ListBuffer[(A, Int)]((start, 0))
   val costs = mutable.Map[A, Int](start -> 0)
   while (open.nonEmpty) {
     val current = open.minBy(_._2)
     open -= current
     if (current._1 == goal) open.clear
     else {
       val newCost = costs(current._1) + 1
       val neighbors = af(current._1)
       for (next <- neighbors) {
         if (!costs.contains(next) || newCost < costs(next)) {
           costs(next) = newCost
           val priority = newCost + cf(goal, next)
           open += ((next, priority))
         }
       }
     }
   }
   costs.toMap
 }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day15.txt"))
    val map = play(program)
    val goal = map.find(_._2 == 2).get._1
    astar(Square(0, 0), goal)((a, b) => a.x.abs - b.x.abs + a.y.abs - b.y.abs)(s => {
      s.neighbors().map(_._1).filter(n => map.contains(n) && map(n) != 0)
    })(goal)



/*
    val (z, q) = (map.keys.minBy(_.x).x, map.keys.minBy(_.y).y)
    val fixed = map.map(m => (Square(m._1.x + z.abs, m._1.y + q.abs) -> m._2)).toMap
    val (x, y)                 = (fixed.keys.maxBy(_.x).x, map.keys.maxBy(_.y).y)
    val canvas                 = Array.tabulate(y + 1, x + 1)((_, _) => ' ')
    for { p <- fixed } yield canvas(p._1.y)(p._1.x) = p._2 match {
      case 0 => '█'
      case 1 => ' '
      case 2 => 'o'
    }
    canvas foreach { row =>
      row.mkString foreach print; println
    }
*/

  }

}
