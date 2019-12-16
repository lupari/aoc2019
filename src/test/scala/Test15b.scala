import challenge.Day15b
import org.scalatest._

class Test15b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day15b.run() should be(278)
  }
}
