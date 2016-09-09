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
    val itmDiscount = obj.apple.discount
    val numOfItems = 0

    assertResult(0) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of zero oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 0

    assertResult(0) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of one apple at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 1

    assertResult(60) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of one orange at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 1

    assertResult(25) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("a basket containing fifty apples at 60p each at the BOGOF discount rate") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 50

    assertResult(1500) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }
  }

  test("a basket containing fifty oranges at 25p each at the 3for2 discount rate") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 50

    assertResult(850) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }
  }

  /*
   * test discount boundary cases
   */

  test("the total cost of two apples at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 2

    assertResult(60) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of three apples at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 3

    assertResult(120) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of four apples at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 4

    assertResult(120) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of five apples at 60p") {

    val item = obj.apple.name
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 5

    assertResult(180) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of three oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 3

    assertResult(50) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of four oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 4

    assertResult(75) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of five oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 5

    assertResult(100) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of six oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 6

    assertResult(100) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  test("the total cost of seven oranges at 25p") {

    val item = obj.orange.name
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 7

    assertResult(125) { obj.basketItems(item,numOfItems, itmCost, itmDiscount) }

  }

  /*
   * test discountedItem
   */

  test("for zero apples at the BOGOF discounted rate") {
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 0

    assertResult(0) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for one apple at the BOGOF discounted rate") {
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 1

    assertResult(60) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for two apples at the BOGOF discounted rate") {
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 2

    assertResult(60) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for three apples at the BOGOF discounted rate") {
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 3

    assertResult(120) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for four apples at the BOGOF discounted rate") {
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 4

    assertResult(120) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for five apples at the BOGOF discounted rate") {
    val itmCost = obj.apple.price
    val itmDiscount = obj.apple.discount
    val numOfItems = 5

    assertResult(180) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for zero oranges at the 3for2 discounted rate") {
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 0

    assertResult(0) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for one orange at the 3for2 discounted rate") {
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 1

    assertResult(25) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for three oranges at the 3for2 discounted rate") {
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 3

    assertResult(50) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for four oranges at the 3for2 discounted rate") {
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 4

    assertResult(75) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for five oranges at the 3for2 discounted rate") {
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 5

    assertResult(100) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for six oranges at the 3for2 discounted rate") {
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 6

    assertResult(100) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }

  test("for seven oranges at the 3for2 discounted rate") {
    val itmCost = obj.orange.price
    val itmDiscount = obj.orange.discount
    val numOfItems = 7

    assertResult(125) { obj.discountItem(itmCost,numOfItems, itmDiscount) }
  }


  /*
* base tests
*/

  test("a basket containing no items") {

    val basket = List()
    assertResult(0) { obj.totalBasketItems(basket) }

  }

  test("a basket containing zero apples at the BOGOF discount - logical test") {

    val basket = List.fill(0)("Apple")

    assertResult(0) { obj.totalBasketItems(basket) }

  }

  test("a basket containing one apple at the BOGOF discount - lowest denomination") {

    val basket = List.fill(1)("Apple")

    assertResult(60) { obj.totalBasketItems(basket) }

  }

  test("a basket containing two apples") {

    val basket = List.fill(2)("Apple")

    assertResult(60) { obj.totalBasketItems(basket) }

  }

  test("a basket containing three apples") {

    val basket = List.fill(3)("Apple")

    assertResult(120) { obj.totalBasketItems(basket) }

  }

  test("a basket containing zero oranges at the 3for2 discount - logical test") {

    val basket = List.fill(0)("Orange")

    assertResult(0) { obj.totalBasketItems(basket) }

  }

  test("a basket containing one orange at 25p at the 3for2 discount - lowest denomination") {

    val basket = List.fill(1)("Orange")

    assertResult(25) { obj.totalBasketItems(basket) }

  }

  test("a basket containing two oranges") {

    val basket = List.fill(2)("Orange")

    assertResult(50) { obj.totalBasketItems(basket) }

  }

  test("a basket containing three oranges") {

    val basket = List.fill(3)("Orange")

    assertResult(50) { obj.totalBasketItems(basket) }

  }

  test("a basket containing fifty apples at 60p each at the BOGOF discount") {

    val basket = List.fill(50)("Apple")
    assertResult(1500) { obj.totalBasketItems(basket) }
  }

  test("a basket containing fifty oranges at 25p each at the 3for2 discount - a reasonable amount") {

    val basket = List.fill(50)("Orange")

    assertResult(850) { obj.totalBasketItems(basket) }

  }

  /*
   * test a mix of basket items
   */

  test("the cost of one orange at 25p each at the 3for2 discount and three apples at 60p each at the BOGOF discount") {

    val basket = Seq("Apple", "Apple", "Orange", "Apple")

    assertResult(145) { obj.totalBasketItems(basket) }
  }

  /*
   * test a mix of known and unknown basket
   * expectation: unknown items ignored. a report of ignored items
   */

  test("the cost of one orange at 25p each at the 3for2 discount and three apples at 60p each at the BOGOF discount, amongst erroneous items") {

    val basket = Seq("Apple", "Banana", "Apple", "Orange", "Tomato", "Apple", "Pear")

    assertResult(145) { obj.totalBasketItems(basket) }
  }

}