import challenge.Day6
import org.scalatest._

class Test6 extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day6.run() should be(300598)
  }
}
