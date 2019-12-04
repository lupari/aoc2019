import challenge.Day3b
import org.scalatest._

class Test3b extends FlatSpec with Matchers {

  it should "display correct result" in {
    Day3b.run() should be(14746)
  }
}
