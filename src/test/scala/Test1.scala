import org.scalatest._

import challenge.Day1

class Test1 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day1.run() should be(3268951)
  }
}
