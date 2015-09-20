package stores

import java.util.UUID
import actors.{GameState, TurnTimeout}
import controllers.Events.Cards
import scala.concurrent.duration._
import akka.actor.{ActorSystem, ActorRef, Cancellable}
import models.{Deck, Card}

case class CurrentTurn (playerId: UUID, cancellable: Cancellable)

case class Player (position: Int,
                   name: String,
                   cards: List[Card] = List.empty,
                   money: Int = 100,
                   bet: Int = 0,
                   totalBet: Int = 0,
                   folded: Boolean = false) {
  require(position >= 1 && position <= 10, "Maximum postions in a table is 10")
}

class TableStore(tableActor: ActorRef)(implicit system: ActorSystem) {
  type Cards = List[Card]
  import system.dispatcher

  private var players: Map[UUID, Player] = Map.empty // needs an order
  private var dealerPosition: Int = 0
  private var currentTurn: Option[CurrentTurn] = None
  private var deck = new Deck
  private var calledCurrentRound: Seq[UUID] = List.empty
  private var tableCards: Cards = List.empty
  private var gameState: GameState.Value = GameState.Pause

  def roundFinished =
    calledCurrentRound.length == players.values.filterNot(_.folded == true).toList.length

  def firstPositionAfterDealer = firstAfterPosition(dealerPosition)

  def firstAfterDealer = players.find(_._2.position == firstPositionAfterDealer).get._1

  def nextPlayerPosition() = currentTurn match {
    case Some(CurrentTurn(id, _)) => firstAfterPosition(players(id).position)
    case None => firstPositionAfterDealer
  }

  def firstAfter(id: UUID) = {
    val pos = players(id).position
    val next = firstAfterPosition(pos)
    players.find(_._2.position == next).get._1
  }

  def firstAfterCurrent() = currentTurn.map(c => firstAfter(c.playerId)).get

  def firstAfterPosition(position: Int) = {
    val positions = players.map(_._2.position).toList.sorted
    if (position == 0 || position >= positions.max) positions.min
    else positions.dropWhile(_ <= position).head
  }

  def startPreFlop () = {
    calledCurrentRound = List.empty
    deck = new Deck
    tableCards = List.empty
    gameState = GameState.PreFlop

    dealerPosition = firstPositionAfterDealer
  }

  def startFlop () = {
    gameState = GameState.Flop
    calledCurrentRound = List.empty

    players = players.map { case (id, p) =>
      id -> p.copy(totalBet = p.bet, bet = 0)
    }

    tableCards = List(deck.randomCard, deck.randomCard, deck.randomCard)
    tableCards
  }

  def startTurn () = {
    gameState = GameState.Turn
    calledCurrentRound = Nil

    players = players.map { case (id, p) =>
      id -> p.copy(totalBet = p.bet, bet = 0)
    }

    val newCards = List(deck.randomCard)
    tableCards = tableCards ++ newCards
    newCards
  }

  def startRiver () = {
    gameState = GameState.River
    calledCurrentRound = Nil

    players = players.map { case (id, p) =>
      id -> p.copy(totalBet = p.bet, bet = 0)
    }

    val newCards = List(deck.randomCard)
    tableCards = tableCards ++ newCards
    newCards
  }

  def endRiver () = {
    gameState = GameState.Pause
    calledCurrentRound = Nil

    players = players.map { case (id, p) =>
      id -> p.copy(totalBet = p.bet, bet = 0)
    }
  }

  def isTurn(playerId: UUID) = currentTurn match {
    case Some(CurrentTurn(id, cancellable)) if id == playerId => true
    case _ => false
  }

  def finishCurrentTurn() = finishTurn(currentTurn.get.playerId)

  def finishTurn(playerId: UUID): Boolean = currentTurn match {
    case Some(CurrentTurn(id, cancellable)) if id == playerId =>
      cancellable.cancel()
      currentTurn = None
      true

    case _ => false
  }

  def setTurn(playerId: UUID) = {
    require(currentTurn.isEmpty, "Current turn must be empty before assigning new player")
    require(players.contains(playerId), "Player must be part of the table")

    currentTurn = Option(CurrentTurn(
      playerId,
      system.scheduler.scheduleOnce(10 seconds, tableActor, TurnTimeout())
    ))
  }

  def bet = players.map(_._2.bet).max

  def totalBet = players.map(_._2.totalBet).max

  def setBet(playerId: UUID, bet: Int) = {
    require(players.contains(playerId), "Player must be part of the table")

    calledCurrentRound = List(playerId)
    val p = players(playerId)
    players = players.updated(playerId, p.copy(bet = bet, money = p.money + p.bet - bet))
    finishTurn(playerId)

    players(playerId)
  }

  def doCheck (playerId: UUID) = {
    require(players.contains(playerId), "Player must be part of the table")

    calledCurrentRound = calledCurrentRound :+ playerId
    finishTurn(playerId)
  }

  def doFold (playerId: UUID) = {
    require(players.contains(playerId), "Player must be part of the table")

    val p = players(playerId)
    players = players.updated(playerId, p.copy(bet = 0, totalBet = p.bet, folded = true))
    finishTurn(playerId)
  }

  def newPlayer(p: Player): UUID = {
    val id = UUID.randomUUID()
    players += id -> p // not sure if this works
    id
  }

  def dealHand(id: UUID): Cards = {
    require(players.contains(id), "Player must be part of the table")

    val cards = List(deck.randomCard, deck.randomCard)
    players = players.updated(id, players(id).copy(cards = cards))

    cards
  }

  def getPlayers = players

  def getTableCards = tableCards

  def getGameState = gameState

  def randomCard = deck.randomCard

  def reset() = {
    deck = new Deck
    // reset every player

    players = players.map { case (id, player) => id ->
      player.copy(cards = Nil, totalBet = 0)
    }
  }

  def setup = {
    // set dealer
    // get small blind
    // get big blind
  }

}