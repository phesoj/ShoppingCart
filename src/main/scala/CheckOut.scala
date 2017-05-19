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
  val TypesOfFruit: Seq[Fruit] = Seq(Apple, Orange)
  def countNumberOfItems(items: Seq[Fruit], item: Fruit): Int = items.count(_ == item)

  def totalPrice(shoppingCart: Seq[Fruit]): BigDecimal = TypesOfFruit.map(typeOfFruit =>
    typeOfFruit.priceWithOffers(countNumberOfItems(shoppingCart, typeOfFruit), typeOfFruit.price)).sum
}
