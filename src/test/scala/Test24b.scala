import challenge.Day24b
import org.scalatest._

class Test24b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {

    Day24b.run() should be(2097)

  }
}
