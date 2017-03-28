import org.scalatest.FunSpec
import org.scalatest.Matchers._

class CartSpec extends FunSpec {

  describe("An Apple") {
    it("should cost 60p") {
      Apple().price shouldEqual 60
    }
  }

  describe("An orange") {
    it("should cost 25p") {
      Orange().price shouldEqual 25
    }
  }

  describe("total price") {
    it("should calculate the cost of all the items ") {
      val exampleTrolley = Seq(Orange(), Apple())
      CheckOut.totalPrice(exampleTrolley) shouldEqual 85
    }
  }

}