import challenge.Day17
import org.scalatest._

class Test17 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day17.run() should be(6520)
  }
}
