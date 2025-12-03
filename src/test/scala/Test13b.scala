import challenge.Day13b
import org.scalatest._

class Test13b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day13b.run() should be(11140)
  }
}
