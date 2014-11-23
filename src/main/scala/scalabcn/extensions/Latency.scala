package scalabcn.extensions

import akka.actor._

import scala.util.Random
import scalabcn.marsrover.actors.MarsRover.SelfDestruct
import scalabcn.marsrover.actors.MarsSatellite.AbortMission
import scalabcn.marsrover.actors.Position

// Extension implementation
class Latency(system: ActorSystem) extends Extension {
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
object Latency extends ExtensionId[Latency] with ExtensionIdProvider {
  override def lookup = Latency

  override def createExtension(system: ExtendedActorSystem) = new Latency(system)
}

import scala.language.implicitConversions

// Implicit conversion class of ActorRef
class LatencyActorRef(latencyExt:Latency, actorRef: ActorRef) {

  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit = {
    if (message.isInstanceOf[Position] || message == SelfDestruct || message == AbortMission){
      latencyExt.tell(actorRef, message)
    }else actorRef ! message
  }

}

// Extension trait
trait LatencyExt { self: Actor =>
  implicit def actorlat(actorRef : ActorRef): LatencyActorRef = {
    new LatencyActorRef(Latency(context.system), actorRef)
  }
}














