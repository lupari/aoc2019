import challenge.Day5
import org.scalatest._

class Test5 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day5.run() should be(3122865)
  }
}
