import org.scalatest.{FlatSpec, Matchers}

class ShoppingCartTest extends FlatSpec with Matchers{

  val A = Item("A", 50)
  val B = Item("B", 30)
  val C = Item("C", 20)
  val D = Item("D", 15)


  "A shoppingList" should "be empty when created" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
  }

  "A shoppingList" should "be able to add one element" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A)
    shoppingCart.shoppingList.size should be(1)
    shoppingCart.shoppingList.toList(0)._1 should be("A")
  }

  "A shoppingList" should "be able to add more than one elements" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A)
    shoppingCart.add(B)
    shoppingCart.add(C)

    shoppingCart.shoppingList.size should be(3)
    shoppingCart.shoppingList.toList(0)._1 should be("A")
    shoppingCart.shoppingList.toList(1)._1 should be("B")
    shoppingCart.shoppingList.toList(2)._1 should be("C")

  }


  "A shoppingCart" should "be able to add more one elements of the same type" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A)
    shoppingCart.add(A)

    shoppingCart.shoppingList.size should be(1)
    shoppingCart.shoppingList.get(A.sku).get.size should be(2)

    val dupItems = shoppingCart.shoppingList.get(A.sku).get.filter(item => item.sku.eq(A.sku)) // check if elements matches the type
    dupItems.size should be(2)

  }

  "A shoppingCart" should "be able to remove an item from list" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A)
    shoppingCart.add(B)
    shoppingCart.add(C)

    shoppingCart.shoppingList.size should be(3)
    shoppingCart.shoppingList.get("A").get.size should be(1)
    shoppingCart.remove(A)
    shoppingCart.shoppingList.get("A").get.size should be(0)

  }

  "A shoppingCart" should "be able to remove an item of same sku from list" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)

    shoppingCart.shoppingList.size should be(1)
    shoppingCart.shoppingList.get("A").get.size should be(3)
    shoppingCart.remove(A)
    shoppingCart.shoppingList.get("A").get.size should be(2)

  }


  "A shoppingCart" should "be able to sum up the price of all elements with no special offer" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A) //50
    shoppingCart.add(A) //50
    shoppingCart.add(A) //50
    shoppingCart.add(B) //30
    shoppingCart.add(C) //20

    implicit val specialOffer: SpecialOffers =  SpecialOffers(Map())

    shoppingCart.calcTotal should be(200)

  }


  "A shoppingCart" should "be able to sum up the price of all elements with special offer" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A) //50
    shoppingCart.add(A) //50
    shoppingCart.add(A) //50
    shoppingCart.add(B) //30
    shoppingCart.add(C) //20

    implicit val specialOffer: SpecialOffers =  SpecialOffers(
      Map(A.sku -> Offer(3, 80))
    )

    shoppingCart.calcTotal should be(130)

  }


  "A shoppingCart" should "be able to sum up the price of all elements with [multiple] special offers" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.shoppingList.size should be(0)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)

    shoppingCart.add(B) //30
    shoppingCart.add(C) //20

    implicit val specialOffer: SpecialOffers =  SpecialOffers(
      Map(A.sku -> Offer(3, 80))
    )

    shoppingCart.calcTotal should be(210)

  }


  "A shoppingList with no special offers" should "give to correct total price" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.add(A)
    shoppingCart.add(B)
    shoppingCart.add(C)
    shoppingCart.add(D)

    implicit val specialOffer: SpecialOffers =  SpecialOffers(
      Map()
    )

    shoppingCart.calcTotal should be(115)
  }

  "A shoppingList with special offers" should "give to correct total price" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A) //80
    shoppingCart.add(B) //30
    shoppingCart.add(C) //20
    shoppingCart.add(D) //15

    implicit val specialOffer: SpecialOffers =  SpecialOffers(
      Map(A.sku -> Offer(3, 80))
    )

    shoppingCart.calcTotal should be(145)
  }


  "A shoppingList with dup special offers" should "give to correct total price" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)

    shoppingCart.add(B)
    shoppingCart.add(C)
    shoppingCart.add(D)

    implicit val specialOffer: SpecialOffers =  SpecialOffers(
      Map(A.sku -> Offer(3, 80))
    )

    shoppingCart.calcTotal should be(225)
  }

  "A shoppingList with more than one special offers" should "give to correct total price" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)
    shoppingCart.add(A)

    shoppingCart.add(B)
    shoppingCart.add(B)

    shoppingCart.add(C)
    shoppingCart.add(D)

    implicit val specialOffer: SpecialOffers =  SpecialOffers(
      Map(A.sku -> Offer(3, 80), B.sku -> Offer(2,50))
    )

    shoppingCart.calcTotal should be(245)
  }


  "A shoppingList with more than one special offers 2" should "give to correct total price" in {
    val shoppingCart = ShoppingCart()
    shoppingCart.add(A)
    shoppingCart.add(A)

    shoppingCart.add(B)
    shoppingCart.add(B)

    shoppingCart.add(C)
    shoppingCart.add(C)

    shoppingCart.add(D)
    shoppingCart.add(D)

    implicit val specialOffer: SpecialOffers =  SpecialOffers(
      Map(
        A.sku -> Offer(2, 80),
        B.sku -> Offer(2,50),
        C.sku -> Offer(2, 30),
        D.sku -> Offer(2, 20)
    ))

    shoppingCart.calcTotal should be(180)
  }



}
