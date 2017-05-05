sealed trait Fruit {
  def price : BigDecimal
  def priceWithOffers: (Int, BigDecimal) => BigDecimal
  def classification: String
}

case class Apple(price: BigDecimal = .60, priceWithOffers: (Int, BigDecimal) => BigDecimal = Offers.buyOneGetOneFree, classification: String = "apple") extends Fruit
case class Orange(price: BigDecimal = .25,  priceWithOffers: (Int, BigDecimal) => BigDecimal = Offers.threeForTwo, classification: String = "orange") extends Fruit

object Offers {
  def buyOneGetOneFree(numberOfItems: Int, price: BigDecimal): BigDecimal = {
    val itemsToChargeFor = (numberOfItems / 2) + (numberOfItems % 2)
    itemsToChargeFor * price
  }

  def threeForTwo(numberOfItems: Int, price: BigDecimal): BigDecimal = {
    val itemsToChargeFor = 2 * (numberOfItems / 3) + (numberOfItems % 3)
    itemsToChargeFor * price
  }
}

object CheckOut {
  val TypesOfFruit: Seq[Fruit] = Seq(Apple(), Orange())
  def countNumberOfItems(items: Seq[Fruit], name: String) : Int = items.filter(item => item.classification.matches(name)).size

  def totalPrice(shoppingCart: Seq[Fruit]) : BigDecimal =  TypesOfFruit.map ( typeOfFruit =>
    typeOfFruit.priceWithOffers(countNumberOfItems(shoppingCart,typeOfFruit.classification), typeOfFruit.price)). sum
}
