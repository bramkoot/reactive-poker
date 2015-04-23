package models

import controllers.ActorMessages._
import org.specs2.mutable._
import org.specs2.specification._
import controllers.ActorMessages.Rank._
import controllers.ActorMessages.Suit._
import PokerHands._

class HandExtractorsSpec extends Specification {

  "Extractors function" should {

    "extract a four of a kind aces with kicker" in new TestScope {
      val cards = List(card(Ace, Spades), card(Ace, Diamonds), card(Ace, Hearts), card(Ace, Clubs), card(Two, Spades),
        card(Four, Spades), card(Jack, Hearts))
      val result = FourOfAKind(List(card(Ace, Spades), card(Ace, Diamonds), card(Ace, Hearts), card(Ace, Clubs)),
        card(Jack, Hearts))

      HandExtractors.extract(cards) must_== result
    }

    "extract a three of a kind tens with kickers" in new TestScope {
      val cards = List(card(Ten, Spades), card(Ten, Diamonds), card(Ten, Hearts), card(Two, Hearts), card(Three, Spades),
        card(Four, Spades), card(Jack, Hearts))
      val result = ThreeOfAKind(List(card(Ten, Spades), card(Ten, Diamonds), card(Ten, Hearts)),
        List(card(Jack, Hearts), card(Four, Spades)))

      HandExtractors.extract(cards) must_== result
    }

    "extract a pair with kickers" in new TestScope {
      val cards = List(card(Ace, Spades), card(Ace, Diamonds), card(Queen, Hearts), card(Ten, Clubs), card(Two, Spades),
        card(Four, Spades), card(Jack, Hearts))
      val result = Pair(List(card(Ace, Spades), card(Ace, Diamonds)), List(card(Queen, Hearts), card(Jack, Hearts),
        card(Ten, Clubs)))

      HandExtractors.extract(cards) must_== result
    }

    "extract two pair with kicker" in new TestScope {
      val cards = List(card(Ace, Spades), card(Ace, Diamonds), card(Queen, Hearts), card(Queen, Clubs), card(Two, Spades),
        card(Four, Spades), card(Jack, Hearts))
      val result = TwoPair(List(card(Ace, Spades), card(Ace, Diamonds)), List(card(Queen, Hearts), card(Queen, Clubs)),
        card(Jack, Hearts))

      HandExtractors.extract(cards) must_== result
    }

    "extract a straight" in new TestScope {
      val cards = List(card(Ace, Spades), card(King, Diamonds), card(Queen, Hearts), card(Ten, Clubs), card(Two, Spades),
        card(Four, Spades), card(Jack, Hearts))
      val result = Straight(List(card(Ace, Spades), card(King, Diamonds), card(Queen, Hearts), card(Jack, Hearts),
        card(Ten, Clubs)))

      HandExtractors.extract(cards) must_== result
    }

    "extract a wheel straight" in new TestScope {
      val cards = List(card(Ace, Spades), card(Three, Diamonds), card(Four, Hearts), card(Five, Clubs), card(Two, Spades),
        card(Four, Spades), card(Five, Hearts))
      val result = Straight(List(card(Five, Clubs), card(Four, Hearts), card(Three, Diamonds), card(Two, Spades),
        card(Ace, Spades)))

      HandExtractors.extract(cards) must_== result
    }

    "extract a straight flush" in new TestScope {
      val cards = List(card(Ace, Spades), card(Two, Spades), card(Queen, Spades), card(Ten, Spades), card(Nine, Spades),
        card(Eight, Spades), card(Jack, Spades))
      val result = StraightFlush(List(card(Queen, Spades), card(Jack, Spades), card(Ten, Spades), card(Nine, Spades),
        card(Eight, Spades)))

      HandExtractors.extract(cards) must_== result
    }

    "extract a flush" in new TestScope {
      val cards = List(card(Ace, Spades), card(Two, Spades), card(Queen, Spades), card(Ten, Spades), card(Nine, Hearts),
        card(Eight, Spades), card(Jack, Spades))
      val result = Flush(List(card(Ace, Spades), card(Queen, Spades), card(Jack, Spades), card(Ten, Spades),
        card(Eight, Spades)))

      HandExtractors.extract(cards) must_== result
    }

    "extract a royal straight flush" in new TestScope {
      val cards = List(card(Ace, Hearts), card(Two, Spades), card(Queen, Hearts), card(Ten, Hearts), card(King, Hearts),
        card(Eight, Spades), card(Jack, Hearts))
      val result = StraightRoyalFlush(List(card(Ace, Hearts), card(King, Hearts), card(Queen, Hearts), card(Jack, Hearts),
        card(Ten, Hearts)))

      HandExtractors.extract(cards) must_== result
    }

    "extract a high card" in new TestScope {
      val cards = List(card(Ace, Hearts), card(Two, Spades), card(Three, Spades), card(Ten, Hearts), card(King, Hearts),
        card(Eight, Spades), card(Jack, Hearts))
      val result = HighCard(List(card(Ace, Hearts), card(King, Hearts), card(Jack, Hearts),
        card(Ten, Hearts), card(Eight, Spades)))

      HandExtractors.extract(cards) must_== result
    }
  }

  trait TestScope extends Scope {
    def card(rank: Rank.Value, suit: Suit.Value) = Card(suit.id*13+rank.id)
  }

}
