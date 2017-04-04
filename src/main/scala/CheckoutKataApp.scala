/**
  * Scala version of a supermarket checkout that calculates the total price of the number of items
  */
object CheckoutKataApp extends App{

  // Items to buy
  val A = Item("A", 50)
  val B = Item("B", 30)
  val C = Item("C", 20)
  val D = Item("D", 15)

  // Special offer for items
  implicit val todaySpecialOffer: SpecialOffers =  SpecialOffers(
    Map((A.sku -> Offer(3, 130)),
      (B.sku -> Offer(2, 45))
    )
  )

  // Adding items to shopping list
  val shoppingList1 = new ShoppingCart(Map.empty)
  val newShoppingList = shoppingList1.add(A).add(B).add(B)

//  shoppingList1.calcTotal
  newShoppingList.printPretty // print pretty shopping list and the total

}
