import challenge.Day12b
import org.scalatest._

class Test12b extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day12b.run() should be(326365108375488L)
  }
}
