import challenge.Day23
import org.scalatest._

class Test23 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day23.run() should be(1644352419829L)

  }
}
