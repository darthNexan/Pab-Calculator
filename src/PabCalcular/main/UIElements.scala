package PabCalcular.main

import PabMath.util.Equation
import PabTools.StringManipulations._

import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.control._
import scalafx.scene.layout._

/**
  * Created by dennis on 8/2/16.
  */

//TODO: Comment the UI Elements before I forget
object UIElements {

  var evaluated:Boolean = false
  val functions: List[String] = List ("log","ln","\u23B7")
  val numberModifier:List[String] = List(".","-")
  val suffixOps:List[String]=List("!","^","E")
  val trigFunctions:List[(String, String, String)] = List(("sin","arc sin","csc"),("cos","arc cos","sec"),("tan","arc tan","cot"))
  val arithmeticFunctions: List[String] = List("(",")","+","-","*","/")
  val combPerm: List[String] = List("C","P")
  def lastNum ={
    val numberRegex = """\-?[0-9]+\.?[0-9]*(?:E\-?[0-9]+)?"""
    findLastMatch(textArea.text.value,numberRegex)
  }


  def evaluate():Option[String] = {
    val expr = textArea.text.value
    val res = Equation(expr.replaceAllLiterally("\u03c0", math.Pi.toString).replaceAllLiterally("\u2107",math.E.toString)).evaluate()
    if( res.matches(Equation.number)) Some(res)
    else None
  }


  /**
    * @see Makes the primary input display
    *
    *
    */
  val textArea:TextArea = new TextArea()

  /**
    * Returns an action event that validates and appends any input entered from the keypad by ensuring that if the last value entered was a number a '*' is added.
    * Or if the last entered value was an arithmetic operation the function is added.
    *
    * @param text
    * @return
    */
  private def onFunctionEvent(text:String):(ActionEvent)=>Unit ={
    (e:ActionEvent)=>{
      val (tempNStr,_) = lastNum
      val (tempFnStr,_) = findLastMatch(textArea.text.value,Equation.arithmeticExpression.regex)

      if(textArea.text.value.endsWith(tempNStr) || textArea.text.value.endsWith("!"))
        textArea.text.value = textArea.text.value + '*' + text
      else if(!textArea.text.value.endsWith(tempFnStr))
        textArea.text.value = textArea.text.value + text
    }
  }

  /**
    * Returns an action event that validates the input of any "suffix function"
    *
    * @param text
    * @return
    */
  private def sufFnEvent(text:String):(ActionEvent)=>Unit={
    (e:ActionEvent)=>{
      val (tempNStr,_) = lastNum
      if(textArea.text.value.endsWith(tempNStr) && !textArea.text.value.contains(text))
        textArea.text.value = textArea.text.value + text
    }
  }

  /**
    * Returns an action event that appends a number to the text area.
    *
    * @param text
    * @return
    */
  private def onNumberEvent(text:String):ActionEvent => Unit ={
    (e:ActionEvent) =>{
        textArea.text.value = textArea.text.value +text
    }
  }

  /**
    * Returns an action event that validates the input of '0', '-', '.'
    *
    * @param text
    * @return
    */
  private def onSNumberEvent(text:String):(ActionEvent)=> Unit  ={
    text match {
      case "-" =>
        println("Match1")
        (e:ActionEvent) =>{
          val (tempStr, tempInd) = lastNum
          if (!textArea.text.value.endsWith(tempStr) && !textArea.text.value.endsWith("--"))
            textArea.text.value = textArea.text.value + '-'
          //checks if the - is preceded by an operation and avoids repeated - signs
          else if (!tempStr.contains('-'))
            textArea.text.value = textArea.text.value.take(tempInd - 1) + '-' + tempStr
          //checks to see if the value is signed
          else if (tempStr.contains('-'))
            textArea.text.value = textArea.text.value.take(tempInd-1) + tempStr.tail
          //removes the sign
        }
      case "." =>
        println("Match2")

        (e:ActionEvent) =>{
          val temp = lastNum
          if (textArea.text.value.endsWith(temp._1) && !temp._1.contains('.'))
            textArea.text.value = textArea.text.value + '.'
          //validates '.'

        }
      case "0" =>
        println("Match3")

        (e:ActionEvent)=>{
          val temp = lastNum
          if (temp._1 != "0" || textArea.text.value.isEmpty)
            textArea.text.value = textArea.text.value + '0'
          //validates '0'
        }
    }
  }
  /** Sets the button size, needs to be refactored eventually
    *
    * @return
    */

  private def setButtonSize(height:Int,width:Int,button:ButtonBase) ={
    button.minWidth =width
    button.maxWidth =width
    button.prefWidth =width
    button.minHeight= height
    button.maxHeight= height
    button.prefWidth =height
  }

  lazy val numPane = new VBox{

    children ={
      val numRows = for (x<-9 to 3 by -3) yield{
        new HBox{
          children = {
            val res =for (y <- 2 to 0 by -1) yield {
              new Button((x - y).toString)
            }
            res.map{
              (button)=>{
                // println(button.text.value) //Debugging purposes
                button.onAction = onNumberEvent(button.text.value)
                setButtonSize(50,50,button)
                button
              }
            }
          }
        }
      }
      numRows.toList:::List{
        new HBox{
          val buttonTypes = List("0",".","-")
          children ={
            val res = for(x<-buttonTypes) yield {
              new Button(x)
            }

            res.map{
              (button)=> {
                button.onAction = onSNumberEvent(button.text.value)
                setButtonSize(50, 50, button)
                button
              }
            }
          }
        }
      }
    }

 }

  lazy val arithmeticPane: VBox = new VBox {
    children = {
      val res = for (x <- arithmeticFunctions.indices by 2) yield {
        new HBox {
          children = for (y <- x to x + 1) yield {
            val temp = new Button(arithmeticFunctions(y))
            temp.minWidth =50
            temp.minHeight =50
            temp.onAction=(e:ActionEvent)=>{

              val (temp1,_) = lastNum
              //prevents repeated operation signs
              if (textArea.text.value.endsWith(temp1))
                textArea.text = textArea.text.value + temp.text.value

            }
            temp
          }
        }
      }

      res.toList:::List{
        new HBox{
          children={
            val temp = new Button("=")
            temp.minWidth=100
            temp.minHeight=50
            temp.onAction =(e:ActionEvent) => {
              val res:Option[String] = evaluate()
              res match {
                case Some(s) => textArea.text.value=s
                  evaluated=true
                case None => textArea.text.value="Syntax Error"
                  evaluated=false

              }
            }
            temp
          }
        }
      }


    }

    minWidth = 100
    maxWidth = 100
    prefWidth = 100
    minHeight = 200
    maxHeight = 200
    prefHeight = 200
    layoutX = 0
    layoutY = 0

  }

  lazy val trigButtons:List[Button]= for((x,y,z)<-trigFunctions) yield {
    val temp = new Button {
      text = x
      minWidth =60
      minHeight =30
      onAction = onFunctionEvent(x)
    }
    temp
  }

  def changeMode(changedMode:Int):Unit ={
    val (mode1,mode2,mode3) = trigFunctions.unzip3

    //chooses the new labels for the buttons
    val newMode = changedMode match {
      case 1 => mode1
      case 2 => mode2
      case 3 => mode3

    }
    for(x<-0 to 2){
      trigButtons(x).text.value = newMode(x)
    }

  }

  lazy val unaryButtons: List[Button] = for(x<-suffixOps) yield {
    new Button{
      text= x
      onAction = sufFnEvent(x)
      minWidth = 60
      minHeight = 30

    }
  }

  lazy val othFunctionsButtons: List[Button] = for(x<- functions) yield {
    new Button{
      text = x
      onAction =onFunctionEvent(x)
      minHeight=30
      minWidth =60
    }
  }

  lazy val constantsButtons: List[Button] ={
    def onConstantAction(text:String): (ActionEvent)=>Unit ={
      (e:ActionEvent)=>{
        val (tempNStr,_) = lastNum
       // val (tempFnStr,_) = findLastMatch(textArea.text.value,Equation.arithmeticExpression.regex)

        if(textArea.text.value.endsWith(tempNStr) || textArea.text.value.endsWith("!"))
          textArea.text.value = textArea.text.value + '*' + text
        else textArea.text.value = textArea.text.value + text
      }


    }
    val temp1 = new Button{
      text = "\u03c0"
      onAction  = onConstantAction(text.value)
      minHeight=30
      minWidth =60

    }
    val temp2 = new Button{
      text = "\u2107"
      onAction  = onConstantAction(text.value)
      minHeight=30
      minWidth =60
    }

    temp1::temp2::Nil
  }

  lazy val toggleTrig = new VBox{
    val tog= new ToggleGroup

    children = List(
      new RadioButton{
        text = "Norm"
        toggleGroup = tog
      },
      new RadioButton{
        text = "Inv"
        toggleGroup = tog
      },
      new RadioButton{
        text = "Rec"
        toggleGroup= tog
      })
    tog.selectedToggle.onChange{
      tog.selectedToggle.get().asInstanceOf[javafx.scene.control.ToggleButton].id() match {
        case "Norm" => changeMode(1)
        case "Inv" => changeMode(2)
        case "Rec" => changeMode(3)
      }
    }


  }

  lazy val controlButtons = new VBox{
    children = List(
      new Button{
        text = "CE"
        onAction = (e:ActionEvent) =>{
          textArea.text = ""
        }
      },
      new Button{
        text = "C"
        onAction = (e:ActionEvent) =>{
          textArea.text = textArea.text.value.init
        }
      }
      )
  }

  lazy val pCButtons: List[Button] = for(x<-combPerm) yield {
    new Button{
      text = x
      minHeight = 30
      minWidth = 60
      onAction = (e:ActionEvent) =>{
        val (tempNum,_) = lastNum
        if(textArea.text.value.endsWith(tempNum))
          textArea.text.value = textArea.text.value + text.value
      }
    }
  }


}


