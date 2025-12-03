import challenge.Day3
import org.scalatest._

class Test3 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day3.run() should be(375)
  }
}
