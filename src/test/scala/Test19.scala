import challenge.Day19
import org.scalatest._

class Test19 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day19.run() should be(226)
  }
}
