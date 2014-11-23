package scalabcn.marsrover.actors

import akka.actor.{Terminated, Actor, ActorLogging, Props}
import akka.event.LoggingReceive

import scalabcn.extensions.{WaitForItExt, MarsMap}
import scalabcn.marsrover.actors.MarsRover._
import scalabcn.marsrover.actors.MarsSatellite.{AbortMission, SendLeft, SendRight, StartMission}

object MarsSatellite {
  case object AbortMission
  case object StartMission
  case object SendLeft
  case object SendRight

}

class MarsSatellite(marsMap: MarsMap) extends Actor with ActorLogging with WaitForItExt {
  val rover = context.actorOf(Props(classOf[MarsRover], marsMap), "mars-rover")
  context.watch(rover)
  var lastId = 0

  override def receive = LoggingReceive {
    case StopEngine =>
      rover ! StopEngine

    case AbortMission =>
      rover ! SelfDestruct

    case StartMission =>
      rover ! StartEngine
      rover ! Subscribe

    case SendLeft =>
      rover ! TurnLeft

    case SendRight =>
      rover ! TurnRight

    case pos:Position =>
      marsMap.moveSatellite(pos)
      context.parent ! pos
      log.info(pos.toString)

    case GetPosition =>
      rover ! GetPosition

    case pos: RequestedPosition =>
      context.parent ! pos

    case Terminated(rover) =>
      println("dying...")
      context.stop(self)

  }
}