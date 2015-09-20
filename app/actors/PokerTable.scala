package actors

import java.util.UUID
import akka.actor.{ActorRef, Actor, ActorSystem}
import controllers.Events._
import models.HandExtractors._
import stores.{Player, TableStore}
import controllers.Commands._

case class NextPlayer()
case class TurnTimeout()

object GameState extends Enumeration {
  val Pause, PreFlop, Flop, Turn, River = Value
}

class PokerTableActor(implicit system: ActorSystem) extends Actor {
  import system.dispatcher

  var store = new TableStore(self)
  var players: Map[ActorRef, UUID] = Map.empty
  val smallBlind = 1
  val bigBlind = 2

  def nextRound(): Unit = store.getGameState match {

    case GameState.PreFlop =>
      val cards = store.startFlop()
      store.setTurn(store.firstAfterDealer)

      broadcast(
        RoundEnded(store.totalBet),
        TableCardsAdded(cards),
        PlayerGotTurn(store.firstAfterDealer)
      )

    case GameState.Flop =>
      val cards = store.startTurn()
      store.setTurn(store.firstAfterDealer)

      broadcast(
        RoundEnded(store.totalBet),
        TableCardsAdded(cards),
        PlayerGotTurn(store.firstAfterDealer)
      )

    case GameState.Turn =>
      val cards = store.startRiver()
      store.setTurn(store.firstAfterDealer)

      broadcast(
        RoundEnded(store.totalBet),
        TableCardsAdded(cards),
        PlayerGotTurn(store.firstAfterDealer)
      )

    case GameState.River =>
      store.endRiver()

      broadcast(
        RoundEnded(store.totalBet)
      )

      val hands = store.getPlayers.values.map(p => (p, extract(p.cards ++ store.getTableCards))).toList
      hands.foreach { case (p, h) =>
        broadcast(
          GameMessage(s"player ${p.name} had a $h")
        )
      }

      val sorted = hands.sortBy(_._2.ranking).reverse
      val winners = sorted.filter(a => (a._2 compare sorted.head._2) == 0)
      val amount = Math.floor(store.totalBet / winners.length)

      // TODO unequal split pot

      winners.foreach { case (player, hand) =>
        broadcast(GameMessage(s"${sorted.head._1.name} has won $amount!"))
        // TODO give money to player in store
      }

      store.reset()
  }

  def nextTurnOrNextRound() =
    if (store.roundFinished) nextRound()
    else {
      val next = store.firstAfterCurrent()
      store.setTurn(next)

      broadcast(
        PlayerGotTurn(next)
      )
    }

  def handleActivePlayerCommand(id: UUID, cmd: ActivePlayerCommand) = cmd match {

    case Bet(bet) =>
      val p = store.setBet(id, bet)
      broadcast(PlayerBet(id, p.bet, p.money))

      nextTurnOrNextRound()

    case Check() =>
      if (store.getPlayers(id).bet == store.getPlayers.map(_._2.bet).max) {
        store.doCheck(id)
        broadcast(PlayerChecked(id))

        nextTurnOrNextRound()
      } else {
        sender() ! GameMessage("bet is not maxbet!")
      }

    case Call() =>
      if (store.getPlayers(id).bet < store.bet) {
        val p = store.setBet(id, store.bet)
        broadcast(PlayerCalled(id, p.bet, p.money))

        nextTurnOrNextRound()
      }

    case Fold() =>
      if (store.getPlayers(id).bet < store.bet) {
        store.doFold(id)
        broadcast(PlayerFolded(id))

        nextTurnOrNextRound()
      }
  }

  def receive = {

    case Join(position, name) =>
      val id = store.newPlayer(Player(position, name))
      players += sender() -> id

      println(s"$name is joining the table on position $position")
      broadcast(PlayerAdded(id, name, 100))

    case cmd: ActivePlayerCommand =>
      players.get(sender()) match {
        case Some(id) if store.isTurn(id) =>
          handleActivePlayerCommand(id, cmd)

        case _ =>
          println("Command received from player which turn it is not")
      }

    case StartGame() if store.getPlayers.size < 2 => broadcast(
      GameMessage("The party cannot be started!")
    )

    case StartGame() =>
      store.startPreFlop()

      val p1 = store.firstAfterDealer
      val p2 = store.firstAfter(p1)

      val sb = store.setBet(p1, smallBlind)
      val bb = store.setBet(p2, bigBlind)

      broadcast(
        GameMessage("The party has started!"),
        PlayerBet(p1, sb.bet, sb.money),
        PlayerBet(p2, bb.bet, bb.money)
      )

      players.foreach { case (actor, id) =>
        val hand = store.dealHand(id)
        actor ! PlayerCardsDealt(hand)
      }

      val turn = store.firstAfter(p2)
      store.setTurn(turn)
      broadcast(PlayerGotTurn(turn))

    case TurnTimeout() =>
      broadcast(
        ChatMessage("The current player timed out")
      )
      store.finishCurrentTurn()

      // either check or fold for current player
      // send updates

      // switch turn to new player

    case LeaveUnexpected() =>
      val id = players(sender())
      if (store.isTurn(id)) {
        store.finishCurrentTurn()
        store.setTurn(store.firstAfter(id))
      }
      val p = store.getPlayers(id)
      broadcast(
        GameMessage(s"${p.name} left unexpectedly")
      )
      println(s"${p.name} left the game")

      players = players - sender()
      // update store getPlayers, remove player from table

  }

  def broadcast(m: Event*) = players.keys.foreach { actor =>
    m.foreach(actor ! _)
  }

}