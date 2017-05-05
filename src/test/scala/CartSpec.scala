import org.scalatest.FunSpec
import org.scalatest.Matchers._

class CartSpec extends FunSpec {

  describe("An Apple") {
    it("should cost 60p") {
      Apple().price shouldEqual .60
    }
  }

  describe("An orange") {
    it("should cost 25p") {
      Orange().price shouldEqual .25
    }
  }

  describe("total price") {
    it("should calculate the cost of multiple the items ") {
      val exampleTrolley = Seq(Orange(), Apple())
      CheckOut.totalPrice(exampleTrolley) shouldEqual .85
    }
  }

  describe("buy one get one free on Apples") {
    it("should apply buy one get one free to apples") {
      val twoApples = Seq(Apple(), Apple())
      val threeApples = Seq(Apple(), Apple(), Apple())
      val twoApplesOrange = Seq(Apple(), Apple(), Orange())
      CheckOut.totalPrice(twoApples) shouldEqual .60
      CheckOut.totalPrice(threeApples) shouldEqual 1.20
      CheckOut.totalPrice(twoApplesOrange) shouldEqual .85
    }
  }

  describe("3 for the price of 2 on oranges") {
    it("should apply 3 for the price of 2 on oranges") {
      val threeOranges = Seq(Orange(),Orange(),Orange())
      CheckOut.totalPrice(threeOranges) shouldEqual .50
    }
  }

  describe("should apply multiple offers") {
    it("should apply 3 for the price of 2 on oranges") {
      val threeOranges = Seq(Orange(),Orange(),Orange())
      CheckOut.totalPrice(threeOranges) shouldEqual .50

      val threeApples = Seq(Apple(), Apple(), Apple())
      CheckOut.totalPrice(threeOranges ++ threeApples) shouldEqual 1.70
    }
  }
}