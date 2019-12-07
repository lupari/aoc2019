import challenge.Day7b
import org.scalatest._

class Test7b extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day7b.run() should be(4039164)
  }
}
