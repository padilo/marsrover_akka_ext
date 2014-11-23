package scalabcn.gui

import java.awt.Color
import javax.swing.BorderFactory

import scala.swing._
import scalabcn.extensions.{MarsMap, MarsListener}


class Gui(mars: MarsMap) extends SimpleSwingApplication{
  val stepsColorR = Color.LIGHT_GRAY
  val stepsColorS = Color.LIGHT_GRAY
  val stepsColorE = Color.LIGHT_GRAY

  def missionX = mars.getMissionX
  def missionY = mars.getMissionY

  var running:Boolean = false
  val fontSize = 32
  val labels: Array[Array[Label]] = Array.ofDim[Label](mars.cols, mars.rows)
  var lastRoverLabel:Option[Label] = None

  mars.registerMission(setMission)

  mars.registerEsaListener(
    new MarsListener(
      notifySetPos = labels(_)(_).text += "E",
      notifyRemovePos = (x,y)=>resetLabel(labels(x)(y), "E", stepsColorE)
    )
  )
  mars.registerSatelliteListener(
    new MarsListener(
      notifySetPos = labels(_)(_).text += "S",
      notifyRemovePos = (x,y)=>resetLabel(labels(x)(y), "S", stepsColorS)
    )
  )
  mars.registerRoverListener(
    new MarsListener(
      notifySetPos = (x,y)=> {
        val label = labels(x)(y)
        label.text += "R"
        if(running) label.foreground = Color.GREEN
        else label.foreground = Color.RED

        lastRoverLabel = Some(label)
      },
      notifyRemovePos = (x,y)=>resetLabel(labels(x)(y), "R", stepsColorR)
    )
  )

  mars.registerRunningListeners(newRunningStatus=>{
    if(newRunningStatus != running) {
      for (label <- lastRoverLabel) {
        if (newRunningStatus) {
          label.foreground = Color.GREEN
        } else {
          label.foreground = Color.RED
        }
        label.text = label.text
      }
    }
    running = newRunningStatus
  })


  def resetLabel(label :Label, elem :String="", color :Color=Color.WHITE) = {

    if(running || color==Color.WHITE) label.background = color

    label.foreground = Color.BLACK
    label.opaque = true
    label.text = label.text.replace(elem, "")
  }

  val grid = new GridPanel(mars.cols, mars.rows) {
    for(
      i<-mars.cols-1 to 0 by -1;
      j<-0 until mars.rows
    ) {
      val label = new Label("")

      label.font = new Font("Arial", 0, fontSize)

      resetLabel(label)

      labels(j)(i) = label

      label.border = BorderFactory.createLineBorder(Color.BLACK)
      //s"[$i,$j]")
      contents += label

    }

    setMission(0, 0)
  }
  grid.minimumSize = new Dimension(128, 128)

  private def setMission(oldX:Int, oldY:Int) = {
    val oldMissionLabel = labels(oldX)(oldY)
    oldMissionLabel.border = BorderFactory.createLineBorder(Color.BLACK)

    val missionLabel = labels(missionX)(missionY)
    missionLabel.border = BorderFactory.createLineBorder(Color.GREEN, 8)
    missionLabel.opaque = true

  }

  def top = new MainFrame {
    title = "Mars!"
    contents = grid
    size = new Dimension(512, 512)
  }

  startup(Array())

}

