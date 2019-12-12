import challenge.Day4
import org.scalatest._

class Test4 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day4.run() should be(1694)
  }
}
