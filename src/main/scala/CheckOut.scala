sealed trait Fruit { def price : BigDecimal }

case class Apple(price: BigDecimal = .60) extends Fruit
case class Orange(price: BigDecimal = .25) extends Fruit

object CheckOut {

  def totalPrice(shoppingCart: Seq[Fruit]): BigDecimal = {
    val priceOfApples = buyOneGetOneFree(shoppingCart)
    val priceOfOranges = threeForTwo(shoppingCart)
    priceOfApples + priceOfOranges
  }

  def threeForTwo(items: Seq[Fruit]): BigDecimal = {
    val oranges = items.collect { case orange: Orange => orange }
    val orangesToChargeFor = 2 * (oranges.size / 3 ) + (oranges.size % 3)
    orangesToChargeFor * Orange().price
  }

  private def buyOneGetOneFree(items: Seq[Fruit]): BigDecimal = {
    val apples = items.collect { case apple: Apple => apple }
    val applesToChargeFor = (apples.size / 2) + (apples.size % 2)
    applesToChargeFor * Apple().price
  }
}
