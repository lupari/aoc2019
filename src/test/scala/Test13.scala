import challenge.Day13
import org.scalatest._

class Test13 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day13.run() should be(230)
  }
}
