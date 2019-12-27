import challenge.Day22b
import org.scalatest._

class Test22b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day22b.run() should be(1644352419829L)

  }
}
