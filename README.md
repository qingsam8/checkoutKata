# Checkout Application
Checkout shopping cart application written in scala

Supermarket checkout application that calculates the total price of a number of items. Goods are priced individually and some items are multi-priced: buy n of them, and they’ll cost you y. For example, item ‘A’ might cost 50 pence individually, but this week we have a special offer: buy three ‘A’s and they’ll cost you £1.30.
The checkout accepts items in any order. Because the pricing changes frequently, a mechanism is built to pass in a set of pricing rules each time we start handling a checkout transaction. 
