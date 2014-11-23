package scalabcn.marsrover.actors

import akka.actor.{Actor, ActorLogging, Props, Terminated}
import akka.event.LoggingReceive

import scalabcn.marsrover.MarsMap
import scalabcn.marsrover.actors.Esa.StartMissionEsa
import scalabcn.marsrover.actors.MarsRover.StopEngine
import scalabcn.marsrover.actors.MarsSatellite.{StartMission, SendLeft, AbortMission, SendRight}
import scala.language.postfixOps

case class TurnDistance(val distance:Int, val pos:Position, val takeDirection:()=>Unit)

class Esa(marsMap: MarsMap) extends Actor with ActorLogging {
  val satellite = context.actorOf(Props(classOf[MarsSatellite], marsMap), "mars-satellite")
  context.watch(satellite)

  implicit val limits = new MarsLimits(marsMap.cols, marsMap.rows)

  override def receive = begin

  def begin:Receive = LoggingReceive {
    case StartMissionEsa(x, y) =>
      context.become(mission(x, y), discardOld = true)
      satellite ! StartMission
  }

  def mission(x:Int, y:Int):Receive = LoggingReceive {
    case pos:Position =>
      log.info(pos.toString)

      marsMap.moveEsa(pos)

      if (pos.x == x && pos.y == y) {
        satellite ! StopEngine
        satellite ! AbortMission
        context.become(shutdown, discardOld = true)
      } else {
        val forward = TurnDistance(distance(pos.move(pos.dir), x, y), pos.move(pos.dir), ()=>Unit)
        val turnRight = TurnDistance(distance(pos.move(pos.dir.right), x, y), pos.move(pos.dir.right), ()=>satellite ! SendRight)
        val turnLeft = TurnDistance(distance(pos.move(pos.dir.left), x, y), pos.move(pos.dir.left), ()=>satellite ! SendLeft)

        List(turnRight, forward).foldLeft(turnLeft) ((acc, election)=>if(acc.distance > election.distance) election else acc).takeDirection()
      }

  }

  def shutdown: Receive = LoggingReceive {
    case Terminated(satellite) =>
      context.stop(self)

    case pos:Position =>
      marsMap.moveEsa(pos)

  }

  private def distance(src: Position, x:Int, y:Int): Int = {
    math.min(math.abs(src.x - x), math.abs(src.x - x + limits.x)) + math.min(math.abs(src.y - y), math.abs(src.y - y + limits.y))
  }
}

object Esa {
  case class StartMissionEsa(x:Int, y:Int)
}