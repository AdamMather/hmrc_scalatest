import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ShoppingCartTest extends FunSuite {

  val obj = ShoppingCart

  test("for an apple") {

    obj.apple.name should equal ("Apple")

  }

  test("for the cost of an apple") {

    obj.apple.price should equal (60)

  }

  test("for an orange") {

    obj.orange.name should equal ("Orange")

  }

  test("for the cost of an orange") {

    obj.orange.price should equal (25)

  }

  /*
   * test boundary cases
   */

  test("the total cost of zero apples at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val numOfItems = 0

    assertResult(0) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of one apple at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val numOfItems = 1

    assertResult(60) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of two apples at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val numOfItems = 2

    assertResult(120) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of three apple at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val numOfItems = 3

    assertResult(180) { obj.basketItems(item,numOfItems, itmCost) }

  }


  test("the total cost of zero oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val numOfItems = 0

    assertResult(0) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of one orange at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val numOfItems = 1

    assertResult(25) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of two oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val numOfItems = 2

    assertResult(50) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of three oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val numOfItems = 3

    assertResult(75) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of fifty apples at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val numOfItems = 50

    assertResult(3000) { obj.basketItems(item,numOfItems, itmCost) }

  }

  test("the total cost of fifty oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val numOfItems = 50

    assertResult(1250) { obj.basketItems(item,numOfItems, itmCost) }

  }

  /*
   * base tests
   */

  test("a basket containing no items") {

    val basket = List()
    assertResult(0) { obj.totalBasketItems(basket) }

  }

  test("a basket containing zero apples - logical test") {

    val basket = List.fill(0)("Apple")

    assertResult(0) { obj.totalBasketItems(basket) }

  }

  test("a basket containing one apple - lowest denomination") {

    val basket = List.fill(1)("Apple")

    assertResult(60) { obj.totalBasketItems(basket) }

  }

  test("a basket containing two apples") {

    val basket = List.fill(2)("Apple")

    assertResult(120) { obj.totalBasketItems(basket) }

  }

  test("a basket containing three apples") {

    val basket = List.fill(3)("Apple")

    assertResult(180) { obj.totalBasketItems(basket) }

  }

  test("a basket containing zero oranges - logical test") {

    val basket = List.fill(0)("Orange")

    assertResult(0) { obj.totalBasketItems(basket) }

  }

  test("a basket containing one orange - lowest denomination") {

    val basket = List.fill(1)("Orange")

    assertResult(25) { obj.totalBasketItems(basket) }

  }

  test("a basket containing two oranges") {

    val basket = List.fill(2)("Orange")

    assertResult(50) { obj.totalBasketItems(basket) }

  }

  test("a basket containing three oranges") {

    val basket = List.fill(3)("Orange")

    assertResult(75) { obj.totalBasketItems(basket) }

  }

  test("a basket containing fifty apples - a reasonable amount") {

    val basket = List.fill(50)("Apple")

    assertResult(3000) { obj.totalBasketItems(basket) }
  }

  test("a basket containing fifty oranges - a reasonable amount") {

    val basket = List.fill(50)("Orange")

    assertResult(1250) { obj.totalBasketItems(basket) }

  }

  /*
   * test a mix of basket items
   */

  test("the cost of one orange at 25p each and three apples at 60p each") {

    val basket = Seq("Apple", "Apple", "Orange", "Apple")

    assertResult(205) { obj.totalBasketItems(basket) }
  }

  /*
   * test a mix of known and unknown basket items
   * expectation: unknown items ignored. a report of ignored items
   */

  test("the cost of one orange at 25p each and three apples at 60p each amongst erroneous items") {

    val basket = Seq("Apple", "Banana", "Apple", "Orange", "Tomato", "Apple", "Pear")

    assertResult(205) { obj.totalBasketItems(basket) }
  }

}