package controllers

import akka.actor.{Actor, Props}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api._
import play.api.mvc._
import akka.actor._
import play.api.Play.current
import plugins.TableBoot
import ActorMessages._
import play.api.libs.json._
import scala.math._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def websocket = WebSocket.acceptWithActor[String, JsValue] { req => out =>
    Props(new PokerPlayerActor(out))
  }

}

object ActorMessages {
  case class Card (id: Int) extends Ordered[Card] {
    require(id >= 0 && id <= 51, "Cardnumber must be between 0 and 51")

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

  case class Join (name: String)

  case class Check ()
  case class Fold ()
  case class Bet (money: Int) // or raise
  case class CallIt ()

  case class Hand (cards: List[Card]) {
    require(cards.length == 2, "A hand must consist of 2 cards")
  }
  implicit val handWrites = new Writes[Hand] {
    def writes(p: Hand) = Json.obj(
      "event" -> "hand",
      "data" -> p.cards.map(_.id)
    )
  }

  case class TableCards (cards: List[Card])
  implicit val tableCardWrites = new Writes[TableCards] {
    def writes(t: TableCards) = Json.obj(
      "event" -> "tablecards",
      "data" -> t.cards.map(_.id)
    )
  }

  case class TablePot (pot: Int)
  implicit val tablePotWrites = new Writes[TablePot] {
    def writes(t: TablePot) = Json.obj(
      "event" -> "tablepot",
      "data" -> t.pot
    )
  }

  case class Message (time: DateTime, message: String)
  implicit val messageWrites = new Writes[Message] {
    def writes(m: Message) = Json.obj(
      "event" -> "message",
      "data" -> Json.obj(
        "time" -> DateTimeFormat.forPattern("H:m:s").print(m.time),
        "text" -> m.message
      )
    )
  }

  case class NewPlayer (players: List[(String, Int, Int, Boolean)])
  implicit val newPlayerWrites = new Writes[NewPlayer] {
    def writes(p: NewPlayer) = Json.obj(
      "event" -> "newplayer",
      "data" -> p.players.map { a =>
        Json.obj(
          "name" -> a._1,
          "bet" -> a._2,
          "money" -> a._3,
          "hasTurn" -> a._4
        )
      }
    )
  }
  case class StartGame()

  case class PlayerCard (card: Card)
  case class TableCard (card: Card)
  case class PlayerBet (player: Int, bet: Int)
  case class PlayerTurn (player: Int)
  case class PlayerYourTurn (player: Int)
  case class LeaveUnexpected()
}


class PokerPlayerActor(client: ActorRef) extends Actor {
  lazy val table = Play.current.plugin[TableBoot]
    .getOrElse(throw new RuntimeException("Actors plugin not loaded")).tableActor

  def receive = {
    case m: String =>
      (Json.parse(m) \ "event").asOpt[String].map {
        case "join" =>
          (Json.parse(m) \ "data" \ "name").asOpt[String].map { name =>
            client ! Json.obj("event" -> "reply", "data" -> m)
            table ! Join(name)
          }
        case "bet" =>
          (Json.parse(m) \ "data" \ "bet").asOpt[Int].map { bet =>
            println("received bet")
            table ! Bet(bet)
          }
        case "start" => table ! StartGame()
        case "call" => table ! CallIt()
        case "check" =>
          println("received check message")
          table ! Check()
        case "fold" => table ! Fold()
        case _ => // nothing
      }

    case m: NewPlayer => client ! Json.toJson(m)
    case m: Hand => client ! Json.toJson(m)
    case m: Message => client ! Json.toJson(m)
    case m: TableCards => client ! Json.toJson(m)
    case m: TablePot => client ! Json.toJson(m)
  }

  override def postStop () = {
    table ! LeaveUnexpected()
  }
}
