import challenge.Day16b
import org.scalatest._

class Test16b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day16b.run() should be(28872305)
  }
}
