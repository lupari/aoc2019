import challenge.Day20
import org.scalatest._

class Test20 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day20.run() should be(476)
  }
}
