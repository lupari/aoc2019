import challenge.Day9
import org.scalatest._

class Test9 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day9.run() should be(3507134798L)
  }
}
