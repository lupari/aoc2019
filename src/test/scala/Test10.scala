import challenge.Day10
import org.scalatest._

class Test10 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day10.run() should be(214)
  }
}
