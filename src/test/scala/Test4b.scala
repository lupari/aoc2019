import challenge.Day4b
import org.scalatest._

class Test4b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day4b.run() should be(1148)
  }
}
