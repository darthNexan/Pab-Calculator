package PabMath.util

/**
  * Created by dennis on 8/5/16.
  */
class AlgebraicEquation(private val s:String) extends Equation(_formula =new StringBuilder(s) ) {

  var substituted: Boolean = false

  /**Utility function, replaces the given term with its value
    *
    * @param term character to be replaced
    * @param value
    */
  @scala.annotation.tailrec
  private def replaceMethod(term:Char, value:String): Unit ={
    if(_formula.contains(term)){
      val temp = _formula.indexOf(term)
      _formula.replace(temp,temp+1,value)
      replaceMethod(term,value)
    }
  }

  /** Used to perform the substitution of all terms
    *
    * @param subs The variables and their value. Variable -> Value
    */
  def substitute(subs:Map[Char,String]): Unit ={

    for(x<-subs){
      replaceMethod(x._1,x._2)
    }
    substituted=true
  }

  /**Evaluates  the expression only if it has been substituted
    *
    * @return The value of the expression
    */
  override def evaluate():String ={
    if(substituted)
    {
      parseEquation()
      _formula.mkString
    }
    else{
      throw new Exception("Error: Variables are not defined")
    }
  }

}

object AlgebraicEquation{
  def apply(algebraicExpression:String): AlgebraicEquation = new AlgebraicEquation(algebraicExpression)
}