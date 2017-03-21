// special offers as a map
case class SpecialOffers(offers: Map[String,Offer])

// quantity - the amount you should by for a particular price
// price - the price for the quantity bought
case class Offer(quantity: Int, pricedAt: Double)