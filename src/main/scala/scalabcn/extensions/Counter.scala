package scalabcn.extensions

import akka.actor._

import scala.collection.mutable
import scalabcn.extensions.CounterActor.{Print, Count}


class Counter(system: ActorSystem) extends Extension {
  val countActor = system.actorOf(Props[CounterActor])

  def count(msg: Any): Unit = {
    countActor ! Count(msg)
  }

  def print() = {
    countActor ! Print
  }
}

object Counter extends ExtensionId[Counter] with ExtensionIdProvider{
  override def createExtension(system: ExtendedActorSystem) = new Counter(system)

  override def lookup() = Counter
}

object CounterActor {
  case class Count(msg: Any)
  object Print

}

class CounterActor extends Actor with ActorLogging {
  val countMap = mutable.Map[String, Int]()

  override def receive: Receive = {
    case Count(msg) =>
      val clazz = msg.getClass.getSimpleName
      val optCount = countMap.get(clazz)
      val count = optCount.getOrElse(0)

      countMap.put(clazz, count+1)

    case Print =>
      countMap.foreach(pair=>log.info(pair.toString))

  }
}

