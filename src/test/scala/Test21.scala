import challenge.{Day20b, Day21}
import org.scalatest._

class Test21 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day21.run() should be(19353619)

  }
}
