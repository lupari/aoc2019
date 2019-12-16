import challenge.Day14b
import org.scalatest._

class Test14b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day14b.run() should be(1993284)
  }
}
