import model.Item

object ShoppingCart {

  val apple = new Item("Apple", 60, 2)
  val orange = new Item("Orange", 25, 3)

  def checkout () : Double = {

    val basket = Seq("Apple", "Apple", "Orange", "Apple")
    val totalCost = totalBasketItems(basket)

    return totalCost
  }

  def discountItem(itmCost:Int, itmNum:Int, itmDiscount:Int): Int = {

    //multiply itmCost by itmNum - total item cost
    val totcost = itmCost * itmNum
    //divide total itmNum by discount offer (3for2 | BOGOF) - number of items to discount
    val discountedItemNumber = itmNum / itmDiscount
    //multiply number of items to discount by cost of item - total cost of discounted items
    val discountedCost = discountedItemNumber * itmCost
    //subtract total cost of items by total cost of discounted items
    val totalDiscountedCost = totcost - discountedCost

    return totalDiscountedCost
  }

  def basketItems (item : String, itmNum : Int, itmCost : Int, discount: Int) : Double = {

    val totCost = discountItem(itmCost, itmNum, discount)

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

      val itmDiscount = key match {
        case "Apple" => apple.discount
        case "Orange" => orange.discount
        case _ => 0
      }

      if (itmCost == 0)
        println("unknown item found: " + key)
      else
        totDiscountedCost += basketItems(key, value, itmCost, itmDiscount)
    }

    return totDiscountedCost
  }

  def main(args: Array[String]): Unit = {
    println("the total cost of the basket items is: £" +checkout())
  }
}


