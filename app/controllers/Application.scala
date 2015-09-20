package controllers

import akka.actor.{Actor, Props}
import play.api._
import play.api.mvc._
import akka.actor._
import play.api.Play.current
import plugins.TableBoot
import controllers.Commands._
import controllers.Events._
import play.api.libs.json._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def websocket = WebSocket.acceptWithActor[String, JsValue] { req => out =>
    Props(new PokerPlayerActor(out))
  }

}

// TODO add proper logging library
class PokerPlayerActor(client: ActorRef) extends Actor {
  lazy val table = Play.current.plugin[TableBoot]
    .getOrElse(throw new RuntimeException("Actors plugin not loaded")).tableActor

  def receive = {
    // receive string (JSON) from client)
    case m: String => Json.parse(m).validate[Command].map { c =>
      println(s"Received command $c")
      table ! c
    } recover {
      case e: JsError => println(s"JsError while parsing command: '$e'")
    }

    // receive event from table actor
    case e: Event => client ! Json.toJson(e)

    case _ => //
  }

  override def postStop () = {
    table ! LeaveUnexpected()
  }
}
