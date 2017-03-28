
sealed trait Offer
case object ThreeForTwo extends Offer
case object BuyOneGetOneFree extends Offer


sealed trait Fruit {
  def price : BigDecimal
  def offer: Offer }

case class Apple(price: BigDecimal = 60, offer: Offer = BuyOneGetOneFree ) extends Fruit
case class Orange(price: BigDecimal = 25, offer: Offer = ThreeForTwo ) extends Fruit

object CheckOut {

  def totalPrice(shoppingCart: Seq[Fruit]): BigDecimal = {
    val priceOfApples = buyOneGetOneFree(shoppingCart)
    val priceOfOranges = shoppingCart.collect { case _: Orange => Orange }.map(_.apply().price).sum
    priceOfApples + priceOfOranges
  }

  private def buyOneGetOneFree(items: Seq[Fruit]): BigDecimal = {
    val apples = items.collect { case apple: Apple => apple }
    val applesToChargeFor = (apples.size / 2) + (apples.size % 2)
    applesToChargeFor * Apple().price
  }
}
