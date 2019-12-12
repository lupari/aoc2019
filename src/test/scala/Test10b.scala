import challenge.Day10b
import org.scalatest._

class Test10b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day10b.run() should be(502)
  }
}
