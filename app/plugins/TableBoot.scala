package plugins

import controllers.ActorMessages._
import org.joda.time.DateTime
import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import java.util.UUID
import java.util.UUID._
import views.html.index

import scala.concurrent.duration._
import scala.util.Random
import akka.actor.Actor
import akka.actor.Props
import models.HandExtractors._

class TableBoot(implicit app: Application) extends Plugin {
  implicit val actorSystem = ActorSystem("humor")
  lazy val tableActor = Akka.system.actorOf(Props(new PokerTableActor), "poker-table")
}

class PokerTableActor(implicit system: ActorSystem) extends Actor {
  import system.dispatcher

  case class Player (id: UUID,
                     actor: ActorRef,
                     name: String,
                     cards: List[Card] = List.empty,
                     money: Int = 100,
                     bet: Int = 0,
                     totalbet: Int = 0,
                     folded: Boolean = false)

  var players: List[Player] = List.empty
  var tableCards: List[Card] = List.empty
  var gameState: GameState.Value = GameState.Pause
  var currentTurn: Option[UUID] = None
  var currentTurnCancellable: Option[Cancellable] = None
  var deck = new Deck
  val smallBlind = 1
  val bigBlind = 2
  var calledCurrentRound: List[UUID] = List.empty

  object GameState extends Enumeration {
    val Pause, PreFlop, Flop, Turn, River = Value
  }

  class Deck {
    var cards: List[Card] = List.empty

    def randomCard: Card = {
      val available = (0 to 51).filterNot(n => cards.exists(_.id == n))
      if (available.length == 0) throw new RuntimeException("There are no cards available for drawing")

      val card = Card(available(Random.nextInt(available.length)))
      cards = cards :+ card
      card
    }
  }

  case class NextPlayer()
  case class TurnTimeout()

  def nextPlayer = {
    currentTurn match {
      case Some(id) =>
        val index = players.indexOf(players.find(_.id == id).get)
        if (index+1 >= players.length) players(0).id
        else players(index+1).id
      case None => players(0).id
    }
  }

  def nextRound: Unit = gameState match {
    case GameState.PreFlop =>
      gameState = GameState.Flop
      tableCards = List(deck.randomCard, deck.randomCard, deck.randomCard)
      players.map(_.actor ! TableCards(tableCards))

      calledCurrentRound = List.empty

      currentTurn = Some(players(0).id)
      currentTurnCancellable = Some(system.scheduler.scheduleOnce(10 seconds, self, TurnTimeout()))

      players = players.map(p => p.copy(totalbet = p.bet, bet = 0))
      updateTablePot
      sendPlayerUpdates

    case GameState.Flop =>
      gameState = GameState.Turn
      tableCards = tableCards :+ deck.randomCard
      players.map(_.actor ! TableCards(tableCards))
      calledCurrentRound = List.empty

      currentTurn = Some(players(0).id)
      currentTurnCancellable = Some(system.scheduler.scheduleOnce(10 seconds, self, TurnTimeout()))

      players = players.map(p => p.copy(totalbet = p.bet, bet = 0))
      updateTablePot
      sendPlayerUpdates

    case GameState.Turn =>
      gameState = GameState.River // actually: showdown
      tableCards = tableCards :+ deck.randomCard
      players.map(_.actor ! TableCards(tableCards))
      calledCurrentRound = List.empty

      currentTurn = Some(players(0).id)
      currentTurnCancellable = Some(system.scheduler.scheduleOnce(10 seconds, self, TurnTimeout()))

      players = players.map(p => p.copy(totalbet = p.totalbet + p.bet, bet = 0))
      updateTablePot
      sendPlayerUpdates

    case GameState.River =>
      gameState = GameState.Pause // actually: showdown
      calledCurrentRound = List.empty
      currentTurn = None

      val hands = players.map(p => (p, extract(p.cards ++ tableCards)))
      hands.map {
        case (p, h) => messageToAll(s"player ${p.name} had a $h")
      }

      val sorted = hands.sortBy(_._2.ranking).reverse
      if (sorted(0)._2.ranking == sorted(1)._2.ranking) {
        messageToAll("The game is a tie!")
      } else {
        messageToAll(s"${sorted(0)._1.name} has won this game!")

        val pot = players.map(_.bet).sum
        val winner = sorted(0)._1
        players = players.updated(players.indexOf(winner), winner.copy(money = winner.money + pot))
        players = players.map(p => p.copy(bet = 0, totalbet = 0))
      }

      updateTablePot
      sendPlayerUpdates

  }

  def nextTurnOrNextRound: Unit =
    if (calledCurrentRound.length == players.length) {
      nextRound
    }
    else {
      currentTurn = Some(nextPlayer)
      currentTurnCancellable = Some(system.scheduler.scheduleOnce(10 seconds, self, TurnTimeout()))

      sendPlayerUpdates
    }

  def updateTablePot: Unit = toAll(TablePot(players.map(_.totalbet).sum))

  def sendPlayerUpdates = toAll(NewPlayer(players.map { p =>
    (p.name, p.bet, p.money, currentTurn.contains(p.id))
  }))

  def messageToAll(m: String) = toAll(Message(DateTime.now, m))

  def toAll(m: Any): Unit = players.map(_.actor ! m)

  def isCurrentTurnPlayer = currentTurn.map(id => players.find(_.id == id).get).exists(_.actor == sender())

  def receive = {

    case Join(name: String) =>
      println(s"$name is joining the table")
      players = players :+ Player(randomUUID(), sender(), name)
      sendPlayerUpdates

    case Bet(money) =>
      currentTurnCancellable.map(_.cancel())

      players = players.map {
        case p if p.actor == sender =>
          calledCurrentRound = p.id :: Nil
          p.copy(bet = money, money = p.money - money)
        case p => p
      }

      sendPlayerUpdates
      nextTurnOrNextRound

    case Check() =>
      currentTurnCancellable.map(_.cancel())
      players.find(_.actor == sender()) match {
        case Some(current) if currentTurn.contains(current.id) =>
          if (current.bet == players.map(_.bet).max) {
            calledCurrentRound = calledCurrentRound :+ current.id
            currentTurnCancellable.map(_.cancel())

            sendPlayerUpdates
            nextTurnOrNextRound
          } else {
            sender() ! Message(DateTime.now, "bet is not maxbet!")
          }
        case _ => // ...
      }

    case CallIt() =>
      currentTurnCancellable.map(_.cancel())
      players.find(_.actor == sender()) match {
        case Some(current) if currentTurn.contains(current.id) =>
          val bet = players.map(_.bet).max
          if (current.bet < bet) {
            players = players.updated(players.indexOf(current), current.copy(bet = bet, money = current.money-bet+current.bet))

            calledCurrentRound = calledCurrentRound :+ current.id
            currentTurnCancellable.map(_.cancel())

            sendPlayerUpdates
            nextTurnOrNextRound
          }
        case _ => // ...
      }

    case Fold() =>
      currentTurnCancellable.map(_.cancel())
      players.find(_.actor == sender()) match {
        case Some(current) if currentTurn.contains(current.id) =>
          val bet = players.map(_.bet).max
          if (current.bet < bet) {
            players = players.updated(players.indexOf(current), current.copy(bet = bet, money = current.money-bet+current.bet))

            calledCurrentRound = calledCurrentRound :+ current.id
            currentTurnCancellable.map(_.cancel())

            sendPlayerUpdates
            nextTurnOrNextRound
          }
        case _ => // ...
      }

    case TurnTimeout() =>
      players.map(_.actor ! Message(DateTime.now, "The current player timeout, the sucker"))

      currentTurnCancellable.map(_.cancel())
      currentTurn = Some(nextPlayer)
      currentTurnCancellable = Some(system.scheduler.scheduleOnce(10 seconds, self, TurnTimeout()))

      players.map(_.actor ! NewPlayer(players.map { p =>
        (p.name, p.bet, p.money, p.id == currentTurn.get)
      }))

    case StartGame() =>
      deck = new Deck
      gameState = GameState.PreFlop
      val p1 = players(0).id
      val p2 = players(1).id
      players = players.map {
        case p if p.id == p1 =>
          p.copy(bet = smallBlind, money = p.money - smallBlind)
        case p if p.id == p2 =>
          p.copy(bet = bigBlind, money = p.money - bigBlind)
        case p => p
      }

      players = players.map { p =>
        val hand = List(deck.randomCard, deck.randomCard)
        p.actor ! Hand(hand)
        p.copy(cards = hand)
      }

      currentTurn = Some(players(0).id)
      currentTurnCancellable = Some(system.scheduler.scheduleOnce(10 seconds, self, TurnTimeout()))

      players.map(_.actor ! NewPlayer(players.map { p =>
        (p.name, p.bet, p.money, p.id == currentTurn.get)
      }))

      players.map(_.actor ! Message(DateTime.now, "the party has started!"))

      // small blinds
      // big blinds
      // first player cards are dealt
      // first player get turn to check, bet, call (timeout: 10 sec)
      // next player...
      // cards are put on the table
      // first player gets turn ...


    case NextPlayer() => ???

    case LeaveUnexpected() =>
      players.find(_.actor == sender()).map { p =>
        players = players.filterNot(_ == p)
        players.map(_.actor ! Message(DateTime.now, s"player ${p.name} left unexpectedly"))
        if (currentTurn.contains(p.id)) {
          currentTurn = Some(nextPlayer)
          currentTurnCancellable = Some(system.scheduler.scheduleOnce(10 seconds, self, TurnTimeout()))
        }
      }


  }


}