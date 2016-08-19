package PabMath.util

import java.io.{BufferedWriter, FileWriter}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by dennis on 8/5/16.
  */
trait History[T] {


  /**@see the index of which to report the value
    *
    */
  protected var histIndex:Int=0
  /**@see the array of previous values
    *
    */
  protected val history:ArrayBuffer[T]= ArrayBuffer()

  /**
    * Protected utility function that allows addition of the value to the history before evaluation
    */
  protected def add()

  def historySize = history.size

  /** Allows toggling through the history until a value is found
    *
    * @return previous value at that index
    * @throws IndexOutOfBoundsException if the value exceeds the size of the array
    */
  def toggleUp():T ={
    if(histIndex+1 < history.length){
      histIndex+=1
      history(histIndex)
    }else throw new IndexOutOfBoundsException
  }

  /** Allows toggling down through the history until a value is found
    *
    * @return previous value at that index
    * @throws IndexOutOfBoundsException if the value exceeds the size of the array
    */
  def toggleDown():T={

    if (histIndex-1 >=0) {
      histIndex -= 1
      history(histIndex)
    }
    else throw new IndexOutOfBoundsException
  }

  /** Returns value at indicated index
    *
    * @param index Value to be returned
    * @return
    */

  def getAt(index:Int=this.histIndex):T={
    this.histIndex=index
    history(this.histIndex)
  }

  /**
    * Resets the index
    */

  def resetPosition(): Unit ={
    histIndex=0
  }

  def resetValue()

  /**
    * Returns the history as a list of values
    *
    * @return
    */
  def historyList() = history.toList

  /**
    * Saves the history to file
    *
    * @param fileName file name to be created or appended to
    */
  def saveToFile(fileName:String) ={
    val writer = new BufferedWriter(new FileWriter(fileName,true))

    try {
      history.foreach {
        (x) => writer.append(x + "\n")
      }
    }
    finally{

      writer.close()
    }
  }
}
