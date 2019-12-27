import challenge.Day22
import org.scalatest._

class Test22 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day22.run() should be(8191)

  }
}
