import org.scalatest._

import challenge.Day8

class Test8 extends FlatSpec with Matchers {

  it should "calculate correct result" in {
    Day8.run() should be(2080)
  }
}
