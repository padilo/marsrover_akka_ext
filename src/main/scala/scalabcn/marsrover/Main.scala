package scalabcn.marsrover

import akka.actor._
import akka.event.LoggingReceive

import scala.util.Random
import scalabcn.gui.Gui
import scalabcn.marsrover.actors.Esa.StartMissionEsa
import scalabcn.marsrover.actors.{Esa}

class Main(mission: StartMissionEsa, marsMap: MarsMap) extends Actor with ActorLogging {
  import scala.concurrent.duration._
  import scala.language.postfixOps
  import context._

  val esa = context.actorOf(Props(classOf[Esa], marsMap), "esa")
  context.watch(esa)

  override def preStart() =
    system.scheduler.scheduleOnce(3 seconds, self, mission)

  override def receive = LoggingReceive {
    case Terminated(_) =>
      context.stop(self)
    case StartMissionEsa(_,_) =>
      esa ! mission
  }
}


object Main {
  var gui:Option[Gui] = None

  def maxX=6
  def maxY=6
  val marsMap = new MarsMap(maxX, maxY)
  val system = ActorSystem("app")

  def init(): Unit = {
    if(gui.isEmpty)
      gui = Some(new Gui(mars=marsMap))
  }

  def doMission(x: Int, y: Int): Unit = {

    println(s"mission $x,$y")
    init()
    marsMap.setMission(x, y)

    system.actorOf(Props(classOf[Main], StartMissionEsa(x , y), marsMap), "main")

  }

  def usage(): Unit = {
    println(s"\nNeed two paramters x [0, $maxX), y [0, $maxY)\n")
  }

  def main(args: Array[String]) {
    val nargs = if(args.isEmpty) {
      Array(Random.nextInt(maxX).toString, Random.nextInt(maxY).toString)
    } else args

    if(nargs.size != 2) usage()
    else
      try {
        val params = nargs.map(_.toInt)
        val x = params(0)
        val y = params(1)

        if(x<0 || x >= maxX || y<0 || y >= maxY) usage()
        else doMission(x,y)

      } catch {
        case e:NumberFormatException =>
          usage()
      }
  }


}
