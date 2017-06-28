import Offers.PriceFunction

object Offers {
  type PriceFunction = (Int, BigDecimal) => BigDecimal
  def buyOneGetOneFree(numberOfItems: Int, price: BigDecimal): BigDecimal = {
    val itemsToChargeFor = (numberOfItems / 2) + (numberOfItems % 2)
    itemsToChargeFor * price
  }

  def threeForTwo(numberOfItems: Int, price: BigDecimal): BigDecimal = {
    val itemsToChargeFor = 2 * (numberOfItems / 3) + (numberOfItems % 3)
    itemsToChargeFor * price
  }

  def noOffers(numberOfItems: Int, price: BigDecimal): BigDecimal = numberOfItems * price
}

abstract class Fruit {
  def price: BigDecimal
  def priceWithOffers: PriceFunction
}

case object Apple extends Fruit {
  override val price: BigDecimal = .60
  override val priceWithOffers: PriceFunction = Offers.buyOneGetOneFree
}

case object Orange extends Fruit {
  override val price: BigDecimal = .25
  override val priceWithOffers: PriceFunction = Offers.threeForTwo
}

object CheckOut {

  def summedItems(shoppingCart: Seq[Fruit]) = shoppingCart.map(item => item -> shoppingCart.count(_ == item)).distinct

  def totalPrice(shoppingCart: Seq[Fruit]): BigDecimal = summedItems(shoppingCart).map {
      case (fruit, totalCount) => fruit.priceWithOffers(totalCount, fruit.price)
    }.sum
}
