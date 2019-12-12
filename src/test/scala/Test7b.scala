import challenge.Day7b
import org.scalatest._

class Test7b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day7b.run() should be(4039164)
  }
}
