package PabCalcular.main
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout._

/**
  * Created by dennis on 8/2/16.
  */
object Main extends JFXApp {

  val rowInfo = new RowConstraints(minHeight = 50, prefHeight = 50, maxHeight = 50)
  val colInfo = new ColumnConstraints(minWidth = 30, prefWidth = 30, maxWidth = 30)

  stage = new PrimaryStage {
    title = "PabCalculator"
    scene = new Scene {
      root = {
        new VBox {
          padding = Insets(12)
          children ={
            val temp = new HBox{
              children = UIElements.numPane::UIElements.arithmeticPane::Nil

            }

            val temp2 = new HBox{
              children = {
                UIElements.textArea.maxWidth= 250
                UIElements.textArea
              }

            }
            temp2::temp::Nil
          }
        }

      }
    }

  }
}