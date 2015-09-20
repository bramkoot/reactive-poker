package models

import scala.math._
import scala.util.Random

case class Card (id: Int) extends Ordered[Card] {
  require(id >= 0 && id <= 51, "Card ID must be between 0 and 51")

  def suit = Suit(floor(id/13).toInt)
  def rank = Rank(id%13)

  override def toString = s"Card($rank, $suit)"

  def compare(that: Card) = -(rank compare that.rank)
}

object Rank extends Enumeration {
  // 0 == 2
  // 12 == Ace
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
}
object Suit extends Enumeration {
  val Spades, Hearts, Diamonds, Clubs = Value
}

class Deck {
  var cards: List[Card] = List.empty

  def randomCard: Card = {
    val available = (0 to 51).filterNot(n => cards.exists(_.id == n))
    if (available.isEmpty) throw new RuntimeException("There are no cards available for drawing")

    val card = Card(available(Random.nextInt(available.length)))
    cards = cards :+ card
    card
  }
}