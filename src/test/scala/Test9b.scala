import challenge.Day9b
import org.scalatest._

class Test9b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day9b.run() should be(84513)
  }
}
