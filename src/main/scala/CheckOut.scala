sealed trait Fruit { def price : BigDecimal }
case class Apple(price: BigDecimal = 60) extends Fruit
case class Orange(price: BigDecimal = 25) extends Fruit

object CheckOut {

  def totalPrice(shoppingCart: Seq[Fruit]): BigDecimal = shoppingCart.map(fruit => fruit.price).sum
}
