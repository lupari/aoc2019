import challenge.{Day21, Day21b}
import org.scalatest._

class Test21b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day21b.run() should be(1142785329)

  }
}
