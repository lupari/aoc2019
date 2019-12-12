import challenge.Day2
import org.scalatest._

class Test2 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day2.run() should be(5290681)
  }
}
