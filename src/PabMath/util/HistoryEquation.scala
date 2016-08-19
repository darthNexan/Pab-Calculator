package PabMath.util

/**
  * Created by dennis on 8/5/16.
  */
class HistoryEquation (_formula:String) extends Equation(new StringBuilder(_formula)) with History[String] with Indexed{
  /**Utility function saves the current value before evaluation.
    *
    */
  protected override def add()={
    history+=_formula.mkString
  }

  /**Evaluates and saves the expression
    *
    * @return The value of the expression
    */
  override def evaluate():String={
    add()
    parseEquation()
    _formula.mkString
  }

  /**Resets the value of the expression with the one at the current index
    *
    */
  override def resetValue(): Unit ={
    formula= history(histIndex)
  }
}

object HistoryEquation{
  /** Factory method
    *
    * @param _formula The expression to be evaluated
    * @return An Equation object
    */
  def apply(_formula:String) = new HistoryEquation(_formula)
}
