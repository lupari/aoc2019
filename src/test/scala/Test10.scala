import challenge.Day10
import org.scalatest._

class Test10 extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day10.run() should be(214)
  }
}
