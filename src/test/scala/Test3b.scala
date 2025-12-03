import challenge.Day3b
import org.scalatest._

class Test3b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day3b.run() should be(14746)
  }
}
