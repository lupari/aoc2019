import challenge.{Day23b, Day24}
import org.scalatest._

class Test24 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day24.run() should be(28778811)

  }
}
