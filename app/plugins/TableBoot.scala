package plugins

import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import akka.actor.Props
import actors.PokerTableActor

class TableBoot(implicit app: Application) extends Plugin {
  implicit val actorSystem = ActorSystem("main")
  lazy val tableActor = Akka.system.actorOf(Props(new PokerTableActor), "poker-table")
}




