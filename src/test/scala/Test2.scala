import challenge.Day2
import org.scalatest._

class Test2 extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day2.run() should be(5290681)
  }
}
