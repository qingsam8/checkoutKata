object ShoppingCart{
  def apply(): ShoppingCart = new ShoppingCart()
}

/**
  * class represents a shopping list, can carry out add, remove and calc calls
  */
class ShoppingCart {

  var shoppingList: Map[String, List[Item]] = Map()

  def add(item: Item): Unit = {
    val updatedList = item :: shoppingList.getOrElse(item.sku, List())
    shoppingList = shoppingList+ (item.sku -> updatedList)
  }

  def remove(item: Item): Unit = {
    val updatedList = shoppingList.get(item.sku).get.drop(1)
    shoppingList = shoppingList+ (item.sku -> updatedList)
  }

  def calcTotal(implicit specialOffer: SpecialOffers): Double =
    shoppingList.map(itemSet => sumPriceForSku(itemSet, specialOffer)).foldLeft(0.0)(_+_)


  /**
    * Sums the price of a set of item based on SKU id (better performance when compared to group function)
    *
    * @param itemSet the set of items
    * @param specialOffer the special offers
    * @return
    */
  private def sumPriceForSku(itemSet: (String, List[Item]) ,specialOffer: SpecialOffers): Double = {

    def sumPrice(list: List[Item]) = list.foldLeft(0.0)((total,itemPrice) => total+itemPrice.price)

    if(specialOffer.offers.contains(itemSet._1)) {
      val offer = specialOffer.offers.get(itemSet._1).get
      val offerQuantity = offer.quantity
      val offerPrice = offer.pricedAt
      val numOfOffer = itemSet._2.size / offerQuantity // number of offer based on item bought

      sumPrice(itemSet._2.drop(numOfOffer * offerQuantity)) + (offerPrice*numOfOffer) // add offer price to remaining items in list
    }
    else
      sumPrice(itemSet._2)
  }



  /**
    * Print a pretty layout of the shopping list and its total
    *
    * @param specialOffer
    */
  def printPretty(implicit specialOffer: SpecialOffers) ={

    // The text for a specific item
    def prettyString(items: (String, List[Item])) = {
      val specialOffer = specialOfferString(items._1)
      s"${items._1}   ${items._2(0).price} x ${items._2.size}   $specialOffer"
    }

    // constructs the special offer part of a string
    def specialOfferString(sku: String)={
      if(specialOffer.offers.contains(sku)) {
        val offer: Offer = specialOffer.offers.get(sku).get
        "|" + offer.quantity + " for the price of " +  offer.pricedAt
      }
      else
        "|"
    }

    shoppingList.foreach(elem => println(prettyString(elem)))
    println("_______________")
    println(s"Total: $calcTotal")
  }



  //  /**
  //    * Sums the price of a set of item based on SKU id
  //    * @param itemSet the set of items
  //    * @param specialOffer the special offers
  //    * @return
  //    */
  //  private def sumPriceForSku(itemSet: (String, List[Item]) ,specialOffer: SpecialOffers) = {
  //
  //    def sumPrice(list: List[Item]) = list.foldLeft(0.0)((total,itemPrice) => total+itemPrice.price)
  //
  //    if(specialOffer.offers.contains(itemSet._1)) {
  //      val offer = specialOffer.offers.get(itemSet._1).get
  //      val offerQuantity = offer.quantity
  //      itemSet._2.grouped(offerQuantity).toList.map(elem =>
  //        if(elem.size == offerQuantity)
  //          offer.pricedAt
  //        else
  //          sumPrice(elem)
  //      )
  //        .foldLeft(0.0)(_+_)
  //    }
  //    else
  //      sumPrice(itemSet._2)
  //
  //  }

}
