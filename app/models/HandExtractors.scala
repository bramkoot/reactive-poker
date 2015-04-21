package models

import controllers.ActorMessages._
import models.PokerHands.{PokerHand, HighCard, FullHouse, StraightRoyalFlush}
import play.libs.F

object PokerHands {

  trait PokerHand[T] extends Ordered[PokerHand] {
    val ranking: Int

    def compareToSame(that: T): Int

    final def compare(that: PokerHand) =
      if (ranking < that.ranking) -1
      else if (ranking > that.ranking) 1
      else compareToSame(that.asInstanceOf[T])
  }

  def compareKickers (cards1: List[Card], cards2: List[Card]) = {
    (cards1 zip cards2).filterNot {
      case (l, r) => l.rank == r.rank
    }.headOption match {
      case Some((l, r)) => if (l.rank > r.rank) 1 else -1
      case None => 0
    }
  }

  case class StraightRoyalFlush(cards: List[Card]) extends PokerHand[StraightRoyalFlush] {
    val ranking = 10

    def compareToSame(that: StraightRoyalFlush) = 0
  }
  case class StraightFlush(cards: List[Card]) extends PokerHand[StraightFlush] {
    val ranking = 9

    def compareToSame(that: StraightFlush) = cards.head.rank compare that.cards.head.rank
  }
  case class Flush(cards: List[Card]) extends PokerHand[Flush] {
    val ranking = 8

    def compareToSame(that: Flush) = compareKickers(cards, that.cards)
  }
  case class FullHouse(three: List[Card], two: List[Card]) extends PokerHand[FullHouse] {
    val ranking = 7

    def compareToSame(that: FullHouse) =
      if (three.head.rank == that.three.head.rank) two.head.rank compare that.two.head.rank
      else three.head.rank compare that.three.head.rank
  }
  case class FourOfAKind(cards: List[Card], kicker: Card) extends PokerHand[FourOfAKind] {
    val ranking = 6

    def compareToSame(that: FourOfAKind) =
      if (cards.head.rank == that.cards.head.rank) kicker.rank compare that.kicker.rank
      else cards.head.rank compare that.cards.head.rank
  }
  case class Straight(cards: List[Card]) extends PokerHand[Straight] {
    val ranking = 5

    def compareToSame(that: Straight) = cards.head.rank compare that.cards.head.rank
  }
  case class ThreeOfAKind(cards: List[Card], kickers: List[Card]) extends PokerHand[ThreeOfAKind] {
    val ranking = 4

    def compareToSame(that: ThreeOfAKind) =
      if (cards.head.rank == that.cards.head.rank) compareKickers(kickers, that.kickers)
      else cards.head.rank compare that.cards.head.rank
  }
  case class TwoPair(first: List[Card], second: List[Card], kicker: Card) extends PokerHand[TwoPair] {
    val ranking = 3

    def compareToSame(that: TwoPair) =
      if (first.head.rank == that.first.head.rank) {
        if (second.head.rank == that.second.head.rank) kicker.rank compare that.kicker.rank
        else second.head.rank compare that.second.head.rank
      } else first.head.rank compare that.first.head.rank

  }
  case class Pair(pair: List[Card], kickers: List[Card]) extends PokerHand[Pair] {
    val ranking = 2

    def compareToSame(that: Pair) =
      if (pair.head.rank == that.pair.head.rank) compareKickers(kickers, that.kickers)
      else pair.head.rank compare that.pair.head.rank
  }
  case class HighCard(kickers: List[Card]) extends PokerHand[HighCard] {
    val ranking = 1

    def compareToSame(that: HighCard) = compareKickers(kickers, that.kickers)
  }
}

object HandExtractors {

  import models.{PokerHands => P}

  def extract(cards: List[Card]) = cards match {
    case Straight(Royal(Flush(flush)))      => P.StraightRoyalFlush(flush)
    case Straight(Flush(flush))             => P.StraightFlush(flush) // @TODO cards are in wrong order if wheel straight
    case FourOfAKind(four, kickers)         => P.FourOfAKind(four, kickers.sortBy(_.rank).reverse.head)
    case ThreeOfAKind(three, Pair(two, _))  => P.FullHouse(three, two)
    case Flush(flush)                       => P.Flush(flush)
    case Straight(straight)                 => P.Straight(straight)
    case ThreeOfAKind(three, kickers)       => P.ThreeOfAKind(three, kickers.sortBy(_.rank).reverse.take(2))
    case Pair(first, Pair(second, kickers)) => P.TwoPair(first, second, kickers.sortBy(_.rank).reverse.head)
    case Pair(pair, kickers)                => P.Pair(pair, kickers.sortBy(_.rank).reverse.take(3))
    case c                                  => P.HighCard(c.sortBy(_.rank).reverse.take(5))
  }

  // returns 5 cards
  object Royal {
    def unapply(cards: List[Card]) = {
      cards.map(_.rank).sorted.reverse match {
        case List(Rank.Ace, Rank.King, Rank.Queen, Rank.Jack, Rank.Ten) => Some(cards)
        case _ => None
      }
    }
  }

  // returns 5 cards
  object Flush {
    def unapply(cards: List[Card]) = cards.groupBy(_.suit).find(_._2.length > 4).map {
      case (suit, list) => list.sortBy(_.rank).reverse.take(5)
    }
  }

  // returns 5 cards
  object Straight {
    def unapply(cards: List[Card]) = {
      val sorted = cards.sortBy(_.rank).reverse
      val straight = sorted.tail.foldLeft(List(sorted.head)) {
        case (r, c) if r.length == 5 || r.head.rank == c.rank => r // straight is complete or skip double
        case (r, c) if c.rank.id-1 == r.head.rank.id => c :: r
        case (r, c) => c :: Nil // next card is not next in straight
      }

      if (straight.length >= 5) {
        Some(straight.take(5))
      }
      else if (straight.length == 4 && sorted.head.rank == Rank.Ace && straight.last.rank == Rank.Two) {
        Some(straight :+ sorted.head)
      } else None
    }
  }

  // returns (cards, kickers)
  object FourOfAKind {
    def unapply(cards: List[Card]) =
      cards.groupBy(_.rank).find(_._2.length == 4).map {
        case (_, list) => (list, cards diff list)
      }
  }

  // returns (cards, kickers)
  object ThreeOfAKind {
    def unapply(cards: List[Card]) =
      cards.groupBy(_.rank).filter(_._2.length == 3).map(_._2).toList.sortBy(_.head.rank).reverse.headOption.map { list =>
        (list, cards diff list)
      }
  }

  // returns (paircards, kickers)
  object Pair {
    def unapply(cards: List[Card]) =
      cards.groupBy(_.rank).filter(_._2.length == 2).map(_._2).toList.sortBy(_.head.rank).reverse.headOption.map { list =>
        (list, cards diff list)
      }
  }

}
