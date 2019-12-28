import challenge.{Day23, Day23b}
import org.scalatest._

class Test23b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day23b.run() should be(14805)

  }
}
