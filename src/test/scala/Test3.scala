import challenge.Day3
import org.scalatest._

class Test3 extends FlatSpec with Matchers {

  it should "display correct result" in {
    Day3.run() should be(375)
  }
}
