import challenge.Day4b
import org.scalatest._

class Test4b extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day4b.run() should be(1148)
  }
}