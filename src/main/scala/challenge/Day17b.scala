package challenge

import base.{Challenge, IntCode => ic}

import scala.annotation.tailrec
import scala.io.Source

object Day17b extends Challenge {

  case class Point(x: Int, y: Int) {
    def neighbors = List(Point(x, y + 1), Point(x, y - 1), Point(x + 1, y), Point(x - 1, y))
    def next(dir: Char): Point = dir match {
      case 'U' => Point(x, y - 1)
      case 'D' => Point(x, y + 1)
      case 'L' => Point(x - 1, y)
      case 'R' => Point(x + 1, y)
    }
  }

  case class Dir(p: Point, dir: Char) {
    def rotate(clockwise: Boolean): Dir = dir match {
      case 'U' => if (clockwise) Dir(Point(p.x + 1, p.y), 'R') else Dir(Point(p.x - 1, p.y), 'L')
      case 'D' => if (clockwise) Dir(Point(p.x - 1, p.y), 'L') else Dir(Point(p.x + 1, p.y), 'R')
      case 'L' => if (clockwise) Dir(Point(p.x, p.y - 1), 'U') else Dir(Point(p.x, p.y + 1), 'D')
      case 'R' => if (clockwise) Dir(Point(p.x, p.y + 1), 'D') else Dir(Point(p.x, p.y - 1), 'U')
    }
  }

  def getGrid(input: List[Int]): Map[Point, Char] = {

    @tailrec
    def acc(xs: List[Int], grid: Map[Point, Char], current: Point): Map[Point, Char] = xs match {
      case h :: t =>
        h.toChar match {
          case '\n' =>
            acc(t, grid, Point(0, current.y + 1))
          case _ =>
            acc(t, grid.updated(current, h.toChar), Point(current.x + 1, current.y))
        }
      case _ => grid
    }

    acc(input, Map.empty.withDefaultValue('.'), Point(0, 0))
  }

  def isIntersection(p: Point, grid: Map[Point, Char]): Boolean = p.neighbors.forall(grid(_) == '#')

  def getPath(grid: Map[Point, Char]): List[Char] = {
    def start = grid.find(_._2 == '^').get

    def nextNeighbor(dir: Dir): Option[Dir] = {
      val neighbors = dir.p.neighbors.filter(grid(_) == '#')
      val nextInDir = dir.p.next(dir.dir)
      if (neighbors.contains(nextInDir)) Some(dir.copy(p = nextInDir))
      else {
        val (left, right) = (dir.rotate(clockwise = false), dir.rotate(clockwise = true))
        if (neighbors.contains(left.p)) Some(left)
        else if (neighbors.contains(right.p)) Some(right)
        else None
      }
    }

    @tailrec
    def acc(dir: Dir, xs: List[Char]): List[Char] = {
      nextNeighbor(dir) match {
        case Some(d) => acc(d, xs :+ d.dir)
        case None    => xs
      }
    }

    acc(Dir(start._1, 'U'), Nil)
  }

  def compress(s: String): List[String] = {
    @tailrec
    def _compress(s: String, dir: Char, acc: List[String]): List[String] = s match {
      case "" => acc
      case _ =>
        val c   = s.head
        val seg = s.takeWhile(_ == s.head)
        val nextD = (dir, c) match {
          case ('L', 'D') => 'L'
          case ('R', 'D') => 'R'
          case ('L', 'U') => 'R'
          case ('L', 'D') => 'L'
          case _          => c
        }
        val nextS: String = nextD.toString + seg.length
        _compress(s.drop(seg.length), c, acc :+ nextS)
    }
    _compress(s, ' ', Nil)
  }

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day17.txt"))
    val output  = ic.execute(ic.Input(program, Nil)).sig.map(_.toInt)

    val grid = getGrid(output)

    val path       = getPath(grid)
    val compressed = compress(path.mkString)
    println(compressed.mkString)

    path
  }

}
