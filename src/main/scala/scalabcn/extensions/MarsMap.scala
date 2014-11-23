package scalabcn.extensions


import scalabcn.marsrover.actors.Position

/**
 * Created by pdiaz on 07/11/2014.
 */
class MarsListener(val notifySetPos:(Int,Int)=>Unit = (x,y)=>Unit, val notifyRemovePos:(Int,Int)=>Unit = (x,y)=>Unit)

class MarsMap(val cols:Int=10, val rows:Int=10) {
  type RoverRunningListener = (Boolean)=>Unit

  def registerMission(listener: (Int,Int)=>Unit) = missionListeners = listener :: missionListeners

  def setMission(x: Int, y: Int) = {
    val oldMissionX = missionX
    val oldMissionY = missionY

    missionX = x
    missionY = y

    missionListeners.foreach(_(oldMissionX, oldMissionY))
  }

  private var lastRover:Option[Position] = None
  private var lastSatellite:Option[Position] = None
  private var lastEsa:Option[Position] = None
  private var roverRunning = false

  private var missionX = 0
  private var missionY = 0

  def getMissionX = missionX
  def getMissionY = missionY

  private var roverListeners: List[MarsListener] = List()
  private var satelliteListeners: List[MarsListener] = List()
  private var esaListeners: List[MarsListener] = List()
  private var roverRunningListeners: List[RoverRunningListener] = List()
  private var missionListeners: List[(Int,Int)=>Unit] = List()

  def registerRoverListener(listener: MarsListener): Unit ={
    roverListeners = listener :: roverListeners
  }
  def registerSatelliteListener(listener: MarsListener): Unit ={
    satelliteListeners = listener :: satelliteListeners
  }
  def registerEsaListener(listener: MarsListener): Unit ={
    esaListeners = listener :: esaListeners
  }
  def registerRunningListeners(listener: RoverRunningListener): Unit = {
    roverRunningListeners = listener :: roverRunningListeners
  }

  def moveRover(pos:Position): Unit = moveAndUpdate(lastRover, pos, roverListeners, () => {
    lastRover = Some(pos)
  })

  def moveSatellite(pos:Position): Unit = moveAndUpdate(lastSatellite, pos, satelliteListeners, () => {
    lastSatellite = Some(pos)
  })

  def moveEsa(pos:Position): Unit = moveAndUpdate(lastEsa, pos, esaListeners, () => {
    lastEsa = Some(pos)
  })

  def resetSatellite(pos: Position): Unit = satelliteListeners.foreach(_.notifyRemovePos(pos.x, pos.y))

  def resetRover(pos: Position): Unit = roverListeners.foreach(_.notifyRemovePos(pos.x, pos.y))

  def resetEsa(pos: Position): Unit = esaListeners.foreach(_.notifyRemovePos(pos.x, pos.y))

  private def moveAndUpdate[T<:Position](lastPos: Option[T], pos: T, listeners: List[MarsListener], updateCall: ()=>Unit): Unit = {
    lastPos.map(pos=>listeners.foreach(_.notifyRemovePos(pos.x, pos.y)))
    updateCall()
    listeners.foreach(_.notifySetPos(pos.x, pos.y))
  }

  def notifyRover(pos: Position): Unit ={
    roverListeners.foreach(_.notifySetPos(pos.x, pos.y))
  }

  def setRoverRunning(running:Boolean): Unit = {
    roverRunning = running
    roverRunningListeners.foreach(_(running))
  }

}
