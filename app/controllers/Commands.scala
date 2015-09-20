package controllers

import play.api.libs.json._
import play.api.libs.json.Reads._

object Commands {

  sealed trait Command
  sealed trait ActivePlayerCommand extends Command
  case class Join(position: Int, name: String) extends Command
  case class StartGame() extends Command
  case class Bet(bet: Int) extends ActivePlayerCommand
  case class Call() extends ActivePlayerCommand
  case class Check() extends ActivePlayerCommand
  case class Fold() extends ActivePlayerCommand
  case class LeaveUnexpected() extends Command

  // --- JSON transformations

  implicit val commandReads = new Reads[Command] {
    def reads(json: JsValue) = (json \ "event").asOpt[String].map {
      case "join" =>
        (json \ "data" \ "name").validate[String].flatMap { name =>
          (json \ "data" \ "position").validate[Int].map(p => Join(p, name))
        }

      case "bet" =>
        (json \ "data" \ "bet").validate[Int].map(Bet.apply)

      case "check" => JsSuccess(Check())
      case "call" => JsSuccess(Call())
      case "fold" => JsSuccess(Fold())
      case "startGame" => JsSuccess(StartGame())

      case cmd => JsError(s"Invalid command name '$cmd'")
    } getOrElse JsError("Command name is not present, but expected")
  }

}