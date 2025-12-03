package challenge

import base.Challenge
import intcode.{IntCode => ic}
import lib.GridImplicits._
import lib.Points._

import scala.annotation.tailrec
import scala.io.Source

object Day17b extends Challenge {

  def getPath(grid: Grid[Char]): List[Char] = {

    def start = grid.find(e => "^><v".contains(e._2)).get
    def symbolToDir(s: Char) = s match {
      case '^' => 'U'
      case 'v' => 'D'
      case '<' => 'L'
      case '>' => 'R'
    }
    def nextNeighbor(dir: Dir): Option[Dir] = {
      val neighbors = dir.p.neighbors.filter(grid(_) == '#')
      List(dir.forward(), dir.rotate(clockwise = false), dir.rotate(clockwise = true))
        .find(d => neighbors.contains(d.p))
    }

    @tailrec
    def acc(dir: Dir, xs: List[Char]): List[Char] = {
      nextNeighbor(dir) match {
        case Some(d) => acc(d, xs :+ d.dir)
        case None    => xs
      }
    }

    acc(Dir(start._1, symbolToDir(start._2)), Nil)
  }

  def compress(s: String): List[String] = {
    @tailrec
    def _compress(s: String, dir: Char, acc: List[String]): List[String] = s match {
      case "" => acc
      case _ =>
        val c   = s.head
        val seg = s.takeWhile(_ == s.head)
        val nextDir = (dir, c) match {
          case ('L', 'D') | ('U', 'L') | ('D', 'R') | ('R', 'U') => 'L'
          case ('D', 'L') | ('L', 'U') | ('R', 'D') | ('U', 'R') => 'R'
          case _                                                 => c
        }
        _compress(s.drop(seg.length), c, acc :+ nextDir.toString + seg.length)
    }
    _compress(s, s.head, Nil)
  }

  def findMovements(path: List[String]): List[List[String]] = {

    @tailrec
    def acc(xs: List[String], ms: List[List[String]]): List[List[String]] = xs match {
      case h :: i :: j :: k :: t =>
        val threes: List[String] = List(h, i, j)
        val fours: List[String]  = threes :+ k
        acc(List(i, j, k) ++ t, ms :+ threes :+ fours)
      case h :: i :: j :: t =>
        val threes: List[String] = List(h, i, j)
        acc(List(i, j) ++ t, ms :+ threes)
      case _ => ms
    }

    @tailrec
    def removeSubList(l: List[String], sl: List[String]): List[String] = l.indexOfSlice(sl) match {
      case -1 => l
      case i  => removeSubList(l.patch(i, Nil, sl.length), sl)
    }

    def isCompleteMatch(path: List[String], candidate: List[List[String]]) =
      candidate.foldLeft(path)((a, b) => removeSubList(a, b)).isEmpty

    acc(path, Nil).combinations(3).find(isCompleteMatch(path, _)).get
  }

  def mainRoutine(path: List[String], movements: List[List[String]]): List[Char] = {
    def indexed(xs: List[String]): List[Int] =
      path.zipWithIndex
        .sliding(xs.length)
        .map(l => (l.head._2, l.map(_._1)))
        .filter(_._2 == xs)
        .toList
        .map(_._1)

    val ia = indexed(movements.head).map((_, 'A'))
    val ib = indexed(movements.drop(1).head).map((_, 'B'))
    val ic = indexed(movements.last).map((_, 'C'))
    (ia ++ ib ++ ic).sortBy(_._1).map(_._2)
  }

  def ascii1(xs: List[Char]): List[Char] = xs.flatMap(List(_) :+ ',').dropRight(1) :+ '\n'
  def asciiN(xs: List[String]): List[Char] = {
    def punctuate(s: String): List[Char] =
      if (s.length == 2) List(s.head, s.last, ',') else List(s.head, ',')

    xs.flatMap(x => List(x.head, x.drop(1))).map(_.toString).flatMap(punctuate).dropRight(1) :+ '\n'
  }

  def dustCount(prog: ic.Program,
                main: List[Char],
                a: List[Char],
                b: List[Char],
                c: List[Char]): Long = {
    val in: List[Char] = (main ++ a ++ b ++ c) ++ List('n', '\n')
    ic.execute(ic.Input(prog.updated(0, 2), in.map(_.toInt))).sig.last
  }

  override def run(): Any = {
    val program   = ic.read(Source.fromResource("day17.txt"))
    val output    = ic.execute(ic.Input(program, Nil)).sig.map(_.toInt)
    val grid      = output.map(_.toChar).toGrid.withDefaultValue('.')
    val path      = compress(getPath(grid).mkString)
    val movements = findMovements(path)
    val seqM      = ascii1(mainRoutine(path, movements))
    val seqA      = asciiN(movements.head)
    val seqB      = asciiN(movements.drop(1).head)
    val seqC      = asciiN(movements.last)

    dustCount(program, seqM, seqA, seqB, seqC)
  }

}
