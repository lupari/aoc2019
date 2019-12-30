import challenge.Day18b
import org.scalatest._

class Test18b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day18b.run() should be(3048)
  }
}
