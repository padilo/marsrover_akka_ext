package scalabcn.extensions

import akka.actor._

import scala.util.Random
import scalabcn.marsrover.actors.MarsRover.SelfDestruct
import scalabcn.marsrover.actors.MarsSatellite.AbortMission
import scalabcn.marsrover.actors.Position

// Extension implementation
class WaitForIt(system: ActorSystem) extends Extension {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  import scala.language.postfixOps

  val min = system.settings.config.getValue("akka.contrib.latency.min").render.toInt
  val variable = system.settings.config.getValue("akka.contrib.latency.var").render.toInt

  def tell(actorRef: ActorRef, message: Any)(implicit sender: ActorRef): Unit ={
    val randVariable = if(variable==0) 0
      else if(variable<0) -Random.nextInt(-variable)
      else Random.nextInt(variable)

    system.scheduler.scheduleOnce(randVariable + min millis, actorRef, message)
  }
}

// Latency ExtensionId & Provider
object WaitForIt extends ExtensionId[WaitForIt] with ExtensionIdProvider {
  override def lookup = WaitForIt

  override def createExtension(system: ExtendedActorSystem) = new WaitForIt(system)
}

// Implicit conversion class of ActorRef
class WaitForItActorRef(latencyExt:WaitForIt, actorRef: ActorRef) {
  import scala.language.implicitConversions

  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit = {
    if (message.isInstanceOf[Position] || message == SelfDestruct || message == AbortMission){
      println("slow down")
      latencyExt.tell(actorRef, message)
    }else actorRef ! message
  }

}

// Extension trait
trait WaitForItExt { self: Actor =>
  import scala.language.implicitConversions

  implicit def actorlat(actorRef : ActorRef): WaitForItActorRef = {
    new WaitForItActorRef(WaitForIt(context.system), actorRef)
  }
}














