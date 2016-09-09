import model.Item

object ShoppingCart {

  val apple = new Item("Apple", 60, 2)
  val orange = new Item("Orange", 25, 3)

  def checkout () : Double = {

    val basket = Seq("Apple", "Apple", "Orange", "Apple")
    val totalCost = totalBasketItems(basket)

    return totalCost
  }

  def basketItems (item : String, itmNum : Int, itmCost : Int) : Double = {

    val totCost = itmCost * itmNum

    return totCost
  }

  def totalBasketItems(cart : Seq[String]) : Double = {

    val cartNumGroupByItems = cart.groupBy(identity).mapValues(_.size)

    var totDiscountedCost = 0.0

    for ((key,value) <- cartNumGroupByItems) {
      val itmCost = key match {
        case "Apple" => apple.price
        case "Orange" => orange.price
        case _ => 0
      }


      if (itmCost == 0)
        println("unknown item found: " + key)
      else
        totDiscountedCost += basketItems(key, value, itmCost)
    }

    return totDiscountedCost
  }

  def main(args: Array[String]): Unit = {
    println("the total cost of the basket items is: £" +checkout())
  }
}


