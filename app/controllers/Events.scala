package controllers

import java.util.UUID

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.json._
import models.Card

object Events {
  type Cards = List[Card]

  sealed trait Event

  case class PlayerAdded(id: UUID, name: String, money: Int) extends Event
  case class PlayerLeft(playerId: UUID) extends Event

  case class PlayerFolded(id: UUID) extends Event
  case class PlayerChecked(id: UUID) extends Event
  case class PlayerCalled(id: UUID, bet: Int, money: Int) extends Event
  case class PlayerBet(id: UUID, bet: Int, money: Int) extends Event
  case class PlayerGotTurn(id: UUID) extends Event
  case class RoundEnded(pot: Int) extends Event
  case class TableCardsAdded(cards: Cards) extends Event
  case class PlayerCardsDealt(cards: Cards) extends Event

  case class GameMessage(m: String) extends Event
  case class ChatMessage(m: String) extends Event

  // --- JSON transformations

  case class EventJson(event: String, data: JsValue)
  implicit val eventJsonWrites = Json.writes[EventJson]

  case class PlayerJson (id: UUID, name: String, money: Int)
  implicit val playerJsonWrites = Json.writes[PlayerJson]

  implicit val cardWrites = new Writes[Card] {
    def writes(c: Card) = Json.toJson(c.id)
  }

  implicit def cardsToJson(x: Cards): JsValue = Json.toJson(x)
  implicit def playerToJson(x: PlayerJson): JsValue = Json.toJson(x)
  implicit def cardToJson(x: EventJson): JsValue = Json.toJson(x)
  implicit def uuidToJson(x: UUID): JsValue = Json.toJson(x)
  implicit def intToJson(x: Int): JsValue = Json.toJson(x)

  implicit val eventWrites = new Writes[Event] {
    def writes(e: Event) = Json.toJson(e match {

      case PlayerAdded(id, name, money) =>
        EventJson("playerAdded", PlayerJson(id, name, money))
      case PlayerLeft(id) =>
        EventJson("playerLeft", id)

      case PlayerFolded(id) => EventJson("playerFolded", id)
      case PlayerChecked(id) => EventJson("playerChecked", id)
      case PlayerCalled(id, bet, money) => EventJson("playerCalled", Json.obj(
        "id" -> id, "bet" -> bet, "money" -> money
      ))
      case PlayerBet(id, bet, money) => EventJson("playerBet", Json.obj(
        "id" -> id, "bet" -> bet, "money" -> money
      ))
      case PlayerGotTurn(id)  => EventJson("playerGotTurn", id)
      case RoundEnded(pot) => EventJson("roundEnded", pot)
      case TableCardsAdded(cards) => EventJson("tableCardsAdded", cards)
      case PlayerCardsDealt(cards) => EventJson("playerCardsDealt", cards)

      case GameMessage(m) => EventJson("gameMessage", Json.obj(
        "time" -> DateTimeFormat.forPattern("H:m:s").print(DateTime.now),
        "text" -> m
      ))
      case ChatMessage(m) => EventJson("chatMessage", Json.obj(
        "time" -> DateTimeFormat.forPattern("H:m:s").print(DateTime.now),
        "text" -> m
      ))
    })
  }


}