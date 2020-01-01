package challenge

import base.{Challenge, IntCode => ic}
import lib.Grids.{Grid, GridInput, Point}

import scala.io.Source

object Day17 extends Challenge {

  def isIntersection(p: Point, grid: Grid[Char]): Boolean = p.neighbors.forall(grid(_) == '#')

  override def run(): Any = {
    val program = ic.read(Source.fromResource("day17.txt"))
    val output  = ic.execute(ic.Input(program, Nil)).sig.map(_.toInt)
    val grid    = GridInput(output.map(_.toChar)).withDefaultValue('.')
    grid.filter(_._2 == '#').keys.filter(isIntersection(_, grid)).toList.map(p => p.x * p.y).sum

  }

}
