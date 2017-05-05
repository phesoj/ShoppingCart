sealed trait Fruit {
  def price : BigDecimal
  def priceWithOffers: (Int, BigDecimal) => BigDecimal
}

case class Apple(price: BigDecimal = .60, priceWithOffers: (Int, BigDecimal) => BigDecimal = Offers.buyOneGetOneFree) extends Fruit
case class Orange(price: BigDecimal = .25,  priceWithOffers: (Int, BigDecimal) => BigDecimal = Offers.threeForTwo) extends Fruit

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
  def countNumberOfItems(items: Seq[Fruit], name: Class[_ <: Fruit]) : Int = items.filter(item => item.getClass.equals(name)).size

  def totalPrice(shoppingCart: Seq[Fruit]) : BigDecimal =  TypesOfFruit.map ( typeOfFruit =>
    typeOfFruit.priceWithOffers(countNumberOfItems(shoppingCart,typeOfFruit.getClass), typeOfFruit.price)).sum
}
