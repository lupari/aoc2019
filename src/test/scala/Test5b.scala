import challenge.Day5b
import org.scalatest._

class Test5b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day5b.run() should be(773660)
  }
}
