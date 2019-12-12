import org.scalatest._

import challenge.Day1b

class Test1b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day1b.run() should be(4900568)
  }
}
