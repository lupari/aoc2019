import challenge.Day4
import org.scalatest._

class Test4 extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day4.run() should be(1694)
  }
}
