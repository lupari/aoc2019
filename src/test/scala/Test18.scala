import challenge.Day18
import org.scalatest._

class Test18 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day18.run() should be(3048)
  }
}
