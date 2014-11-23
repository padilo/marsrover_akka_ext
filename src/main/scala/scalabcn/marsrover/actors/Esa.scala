package scalabcn.marsrover.actors

import akka.actor.{Actor, ActorLogging, Props, Terminated}
import akka.event.LoggingReceive
import akka.stream.scaladsl.Flow
import akka.stream.{MaterializerSettings, FlowMaterializer}

import scalabcn.extensions.{WaitForItExt, MarsMap}
import scalabcn.marsrover.actors.Esa.StartMissionEsa
import scalabcn.marsrover.actors.MarsRover.{RequestedPosition, GetPosition, StopEngine}
import scalabcn.marsrover.actors.MarsSatellite.{SendLeft, AbortMission, SendRight, StartMission}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class TurnDistance(val distance:Int, val pos:Position, val takeDirection:()=>Unit)

class Esa(marsMap: MarsMap) extends Actor with ActorLogging with WaitForItExt {
  val satellite = context.actorOf(Props(classOf[MarsSatellite], marsMap), "mars-satellite")
  context.watch(satellite)


  implicit val limits = new MarsLimits(marsMap.cols, marsMap.rows)

  override def receive = begin



  def begin:Receive = LoggingReceive {
    case StartMissionEsa(x, y) =>
      context.become(mission(x, y), discardOld = true)
      satellite ! GetPosition
  }

  def mission(x: Int, y: Int): Receive = {
    case RequestedPosition(pos) =>
      val toSendOperations = ArrayBuffer[()=>Unit]()

      def recDistance(pos: Position): Unit = {
        val posForward = pos.move(pos.dir)
        val posRight = pos.move(pos.dir.right)
        val posLeft = pos.move(pos.dir.left)

        val forward = TurnDistance(distance(posForward, x, y), posForward, ()=>Unit)
        val turnRight = TurnDistance(distance(posRight, x, y), posRight, ()=>satellite ! SendRight)
        val turnLeft = TurnDistance(distance(posLeft, x, y), posLeft, ()=>satellite ! SendLeft)

        val turnDistance = List(turnRight, forward).foldLeft(turnLeft) ((acc, election)=>if(acc.distance > election.distance) election else acc)

        toSendOperations += turnDistance.takeDirection

        if(turnDistance.distance > 0) recDistance(turnDistance.pos)
      }

      recDistance(pos)

      (()=>satellite ! StartMission) +=: toSendOperations += (()=> {
        satellite ! StopEngine
        satellite ! AbortMission
        context.become(shutdown, discardOld = true)
      })

      import scala.concurrent.duration._

      implicit val mat = FlowMaterializer(MaterializerSettings(context.system))

      val ticks = Flow(0 millis, 500 millis, ()=>"Ticks")
      val ops = Flow(toSendOperations.iterator).toPublisher()

      ticks.zip(ops).map(_._2).foreach(_())

    case pos:Position =>
      marsMap.moveEsa(pos)
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