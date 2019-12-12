import challenge.Day12
import org.scalatest._

class Test12 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day12.run() should be(9493)
  }
}
