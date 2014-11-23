package scalabcn.extensions

import akka.actor._
import scala.collection._


/**
 * Created by pablo on 2/11/14.
 */
class CounterImpl(val system: ExtendedActorSystem) extends Extension {
  val countActor = system.actorOf(Props(classOf[CounterActor]), "counter")

  def increment(msg: Any) = {
    countActor ! msg.getClass
  }

}

// Counter ExtensionId, registers the actor and
object Counter extends ExtensionId[CounterImpl] with ExtensionIdProvider {
  override def lookup = Counter

  override def createExtension(system: ExtendedActorSystem) = {
    new CounterImpl(system)
  }
}


class CounterActor extends Actor with ActorLogging {
  val countMap = new mutable.HashMap[Any, Int]

  override def receive:Receive = {
    case x:Any =>
      log.info(x.toString)

      val count = countMap.getOrElse(x, 0) + 1

      countMap += (x -> count)
  }

  override def postStop() {
    println(countMap)
  }

}
