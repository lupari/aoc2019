import challenge.Day14
import org.scalatest._

class Test14 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day14.run() should be(720484)
  }
}
