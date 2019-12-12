import challenge.Day7
import org.scalatest._

class Test7 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day7.run() should be(22012)
  }
}
