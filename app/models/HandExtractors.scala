package models

import controllers.ActorMessages._
import models.PokerHands._
import play.libs.F

object PokerHands {

  trait PokerHand extends Ordered[PokerHand] {
    val ranking: Int
    def cards: List[Card]

    final def compare(that: PokerHand) =
      if (ranking < that.ranking) -1
      else if (ranking > that.ranking) 1
      else compareKickers(cards, that.cards)

    require(cards.length==5, "A pokerhand should always contain exactly 5 cards")
  }

  def compareKickers (cards1: List[Card], cards2: List[Card]) = {
    (cards1 zip cards2).filterNot {
      case (l, r) => l.rank == r.rank
    }.headOption match {
      case Some((l, r)) => if (l.rank > r.rank) 1 else -1
      case None => 0
    }
  }

  case class StraightRoyalFlush(cards: List[Card]) extends PokerHand {
    val ranking = 10
  }
  case class StraightFlush(cards: List[Card]) extends PokerHand {
    val ranking = 9
  }
  case class Flush(cards: List[Card]) extends PokerHand {
    val ranking = 8
  }
  case class FullHouse(three: List[Card], two: List[Card]) extends PokerHand {
    val ranking = 7
    def cards = three ++ two
  }
  case class FourOfAKind(four: List[Card], kicker: Card) extends PokerHand {
    val ranking = 6
    def cards = four :+ kicker
  }
  case class Straight(cards: List[Card]) extends PokerHand {
    val ranking = 5
  }
  case class ThreeOfAKind(three: List[Card], kickers: List[Card]) extends PokerHand {
    val ranking = 4
    def cards = three ++ kickers
  }
  case class TwoPair(first: List[Card], second: List[Card], kicker: Card) extends PokerHand {
    val ranking = 3
    def cards = first ++ second :+ kicker
  }
  case class Pair(pair: List[Card], kickers: List[Card]) extends PokerHand {
    val ranking = 2
    def cards = pair ++ kickers
  }
  case class HighCard(cards: List[Card]) extends PokerHand {
    val ranking = 1
  }
}

object HandExtractors {

  import models.{PokerHands => P}

  def extract(cards: List[Card]) = cards match {
    case Straight(Royal(Flush(flush)))      => P.StraightRoyalFlush(flush)
    case StraightFlush(flush)               => P.StraightFlush(flush)
    case FourOfAKind(four, kickers)         => P.FourOfAKind(four, kickers.sortBy(_.rank).reverse.head)
    case ThreeOfAKind(three, Pair(two, _))  => P.FullHouse(three, two)
    case Flush(flush)                       => P.Flush(flush)
    case Straight(straight)                 => P.Straight(straight)
    case ThreeOfAKind(three, kickers)       => P.ThreeOfAKind(three, kickers.sortBy(_.rank).reverse.take(2))
    case Pair(first, Pair(second, kickers)) => P.TwoPair(first, second, kickers.sortBy(_.rank).reverse.head)
    case Pair(pair, kickers)                => P.Pair(pair, kickers.sortBy(_.rank).reverse.take(3))
    case c                                  => P.HighCard(c.sortBy(_.rank).reverse.take(5))
  }

  object Royal {
    def unapply(cards: List[Card]) = {
      cards.map(_.rank).sorted.reverse match {
        case List(Rank.Ace, Rank.King, Rank.Queen, Rank.Jack, Rank.Ten) => Some(cards)
        case _ => None
      }
    }
  }

  object StraightFlush {
    def unapply(cards: List[Card]) = cards.groupBy(_.suit).find(_._2.length > 4).flatMap {
      case (_, list) if list.length >= 5 =>
        val sorted = list.sortBy(_.rank).reverse
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
      case _ => None
    }
  }

  // returns 5 cards
  object Flush {
    def unapply(cards: List[Card]) = cards.groupBy(_.suit).find(_._2.length > 4).map {
      case (_, list) => list.sortBy(_.rank).reverse.take(5)
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

  object FourOfAKind {
    def unapply(cards: List[Card]) =
      cards.groupBy(_.rank).find(_._2.length == 4).map {
        case (_, list) => (list, cards diff list)
      }
  }

  object ThreeOfAKind {
    def unapply(cards: List[Card]) =
      cards.groupBy(_.rank).filter(_._2.length == 3).map(_._2).toList.sortBy(_.head.rank).reverse.headOption.map { list =>
        (list, cards diff list)
      }
  }

  object Pair {
    def unapply(cards: List[Card]) =
      cards.groupBy(_.rank).filter(_._2.length == 2).map(_._2).toList.sortBy(_.head.rank).reverse.headOption.map { list =>
        (list, cards diff list)
      }
  }

}
