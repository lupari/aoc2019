import challenge.Day16
import org.scalatest._

class Test16 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day16.run() should be(18933364)
  }
}
