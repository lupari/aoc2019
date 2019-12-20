import challenge.Day11b
import org.scalatest._

class Test11b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "display HAFULAPE" in {
    val canvas = Day11b.run().asInstanceOf[Array[Array[Char]]]
    canvas foreach { row =>
      row.mkString foreach print; println
    }
  }
}
