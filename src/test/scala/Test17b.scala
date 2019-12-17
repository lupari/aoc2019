import challenge.Day17b
import org.scalatest._

class Test17b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day17b.run() should be(6520)
  }
}
