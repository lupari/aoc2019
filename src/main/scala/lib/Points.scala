package lib

object Points {

  case class Point(x: Int, y: Int) {
    def +(p: Point): Point = Point(x + p.x, y + p.y)
    def neighbors          = List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))
    def corners =
      List(Point(x - 1, y - 1), Point(x + 1, y - 1), Point(x - 1, y + 1), Point(x + 1, y + 1))
  }
  object Point {
    def adj: List[Point] = List(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
  }

  case class Dir(p: Point, dir: Char) {
    def forward(): Dir = dir match {
      case 'U' => copy(p = p.copy(y = p.y - 1))
      case 'D' => copy(p = p.copy(y = p.y + 1))
      case 'L' => copy(p = p.copy(x = p.x - 1))
      case 'R' => copy(p = p.copy(x = p.x + 1))
    }
    def rotate(clockwise: Boolean): Dir = dir match {
      case 'U' => if (clockwise) Dir(Point(p.x + 1, p.y), 'R') else Dir(Point(p.x - 1, p.y), 'L')
      case 'D' => if (clockwise) Dir(Point(p.x - 1, p.y), 'L') else Dir(Point(p.x + 1, p.y), 'R')
      case 'L' => if (clockwise) Dir(Point(p.x, p.y - 1), 'U') else Dir(Point(p.x, p.y + 1), 'D')
      case 'R' => if (clockwise) Dir(Point(p.x, p.y + 1), 'D') else Dir(Point(p.x, p.y - 1), 'U')
    }
  }

}
