import challenge.Day8b
import org.scalatest._

class Test8b extends FlatSpec with Matchers {

  it should "display AURCY" in {
    val pic = Day8b.run().asInstanceOf[String]
    println(pic)
  }
}
