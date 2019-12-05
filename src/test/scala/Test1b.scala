import challenge.Day1b
import org.scalatest._

class Test1b extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day1b.run() should be(4900568)
  }
}
