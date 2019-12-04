import challenge.Day2b
import org.scalatest._

class Test2b extends FlatSpec with Matchers {

  it should "display correct result" in {
    Day2b.run() should be(5741)
  }
}
