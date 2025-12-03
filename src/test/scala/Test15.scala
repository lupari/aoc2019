import challenge.Day15
import org.scalatest._

class Test15 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day15.run() should be(244)
  }
}
