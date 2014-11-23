package scalabcn.marsrover.actors

import akka.actor.{Cancellable, ActorRef, ActorLogging, Actor}
import akka.event.LoggingReceive
import akka.persistence.{SaveSnapshotSuccess, RecoveryCompleted, SnapshotOffer, PersistentActor}

import scala.language.postfixOps
import scalabcn.extensions.{WaitForItExt, Counter, MarsMap}
import scalabcn.marsrover.actors.MarsRover._

case class Direction(val name: String, val x: Int, val y: Int) extends Serializable {
  var right: Direction = this
  var left: Direction = this

  def set(left: Direction, right: Direction) {
    this.left = left
    this.right = right
  }

  override def toString = name
}

class MarsLimits(val x: Int, val y: Int)

case class Position(val x: Int, val y: Int, val dir: Direction) {
  def move(dir: Direction)(implicit limits: MarsLimits): Position = {
    val nextx = if (x + dir.x >= 0) (x + dir.x) % limits.x else limits.x - 1
    val nexty = if (y + dir.y >= 0) (y + dir.y) % limits.y else limits.y - 1

    new Position(nextx, nexty, dir)
  }
}

object MarsRover {
  private case object Tick

  case object SelfDestruct
  case object StartEngine
  case object StopEngine
  case object Subscribe
  case object TurnLeft
  case object TurnRight

  case object GetPosition
  case class RequestedPosition(val pos: Position)


  val NORTH = new Direction("North", 0, 1)
  val EAST = new Direction("East", 1, 0)
  val SOUTH = new Direction("South", 0, -1)
  val WEST = new Direction("West", -1, 0)

  NORTH.set(left = WEST, right = EAST)
  EAST.set(left = NORTH, right = SOUTH)
  WEST.set(left = SOUTH, right = NORTH)
  SOUTH.set(left = EAST, right = WEST)

  private case class Event(cmd:Any)

}

case class SnapState(direction: Direction, position: Position)

class MarsRover(marsMap: MarsMap) extends PersistentActor with ActorLogging with WaitForItExt {
  var direction:Direction = NORTH
  var position: Position = new Position(0, 0, direction)
  var subscribers = List[ActorRef]()
  implicit val limits:MarsLimits = new MarsLimits(marsMap.cols, marsMap.rows)

  marsMap.moveRover(position)

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  override def persistenceId: String = "mars-rover"

  override def receiveCommand: Receive = analyzingEnv

  override def receiveRecover: Receive = {
    case Event(StartEngine) =>
      startEngine()
    case Event(Tick) =>
      position = position.move(direction)
      marsMap.moveRover(position)
    case Event(TurnLeft) =>
      direction = direction.left
    case Event(TurnRight) =>
      direction = direction.right
    case SnapshotOffer(metadata, SnapState(snapDirection, snapPosition)) =>
      position = snapPosition
      direction = snapDirection
    case snap: SnapshotOffer =>
      println(snap)
      throw new RuntimeException
    case RecoveryCompleted =>
      marsMap.moveRover(position)
  }

  private def ops: Receive = LoggingReceive {
    case Subscribe =>
      subscribers = sender :: subscribers

    case SelfDestruct =>
      context.stop(self)

    case GetPosition =>
      sender ! RequestedPosition(position)

    case SaveSnapshotSuccess(_) =>
  }

  private def analyzingEnv:Receive = LoggingReceive {
    case StartEngine =>
      startEngine()

  } orElse ops

  private def startEngine(): Unit = {
    val engine = context.system.scheduler.schedule(500 millis, 500 millis, context.self, Tick)
    context.become(running(engine))
    marsMap.setRoverRunning(true)
  }

  private def running(engine:Cancellable):Receive = LoggingReceive {
    case Tick =>
      persist(Event(Tick)) (e=> {
        position = position.move(direction)

        marsMap.moveRover(position)
        Counter(context.system).increment(Tick)

        subscribers.foreach(sender => sender ! position)

        log.info(position.toString)
      })

    case StopEngine =>
      log.info("ENGINE STOPPED")
      engine.cancel()
      marsMap.setRoverRunning(false)
      context.unbecome()

      saveSnapshot(SnapState(direction, position))

    case TurnLeft =>
      persist(Event(Tick)) (e=> {
        direction = direction.left
        Counter(context.system).increment(TurnLeft)
        log.info(s"Turn left.. ${direction.toString}")
      })

    case TurnRight =>
      persist(Event(TurnRight)) (e=> {
        direction = direction.right
        Counter(context.system).increment(TurnRight)
        log.info(s"Turn right.. ${direction.toString}")
      })
  } orElse ops

}
