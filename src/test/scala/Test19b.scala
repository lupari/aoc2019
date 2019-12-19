import challenge.Day19b
import org.scalatest._

class Test19b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day19b.run() should be(7900946)
  }
}
