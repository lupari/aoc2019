import challenge.Day8b
import org.scalatest._

class Test8b extends FlatSpec with Matchers {

  it should "display AURCY" in {
    Day8b.run() should be(
      List(
        "0110010010111000110010001",
        "1001010010100101001010001",
        "1001010010100101000001010",
        "1111010010111001000000100",
        "1001010010101001001000100",
        "1001001100100100110000100"
      )
    )
  }
}
