import challenge.{Day20, Day20b}
import org.scalatest._

class Test20b extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day20b.run() should be(5350)
  }
}
