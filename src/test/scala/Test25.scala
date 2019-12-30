import challenge.Day25
import org.scalatest._

class Test25 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day25.run() should be(16810049)
  }
}
