import challenge.Day6b
import org.scalatest._

class Test6b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day6b.run() should be(520)
  }
}
