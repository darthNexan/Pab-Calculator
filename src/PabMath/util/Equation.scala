package PabMath.util

import scala.math._
import scala.util.matching.{Regex, UnanchoredRegex}
import PabTools.StringManipulations._
/**
  * @author Dennis Guye
  *
  * @
  */
class Equation( protected val _formula: StringBuilder) extends AbstractEquation with Ordered[Equation] {
  println("Running equation constructor")

  /*  TODO: COMMENT THIS BLOODY CODE
  *   TODO: make more of the regex class fields
  *
  * */

  override def toString: String = this._formula.mkString

  /**Sets the new expression used by the equation
    *
    * @param formula New formula to be calculated
    * */
  def formula_=(formula:String) = {

    this._formula.setLength(0)
    this._formula.insert(0,formula)
  }
  /**Gets the expression to be evaluated
    *
    * @return expression
    * */

  def formula = this._formula


  /**Utility function used to parse the internal expression into simpler operations that are easier to evaluate.
    *
    * */


 final protected def parseEquation():Unit={

    if(this._formula.contains('(')){
      extractReplace(_formula,'(',')')(Equation.evaluate)
      parseEquation()
    }
    else{

      _formula.replace(0,_formula.length,Equation.evaluate(_formula.mkString.filter((c:Char)=>
        c!=')' && c!='('
      )))

    }
  }

  /**Evaluates expression and returns the value
    *
    * @return The value of the expression
    * */

  def evaluate():String={

    parseEquation()
    _formula.mkString
  }

/**@see
  * */
  def ==(that:Equation):Boolean ={
    val a = Equation(this._formula.mkString)
    val b = Equation(that._formula.mkString)

    a.evaluate().toDouble == b.evaluate().toDouble
  }

  def compare(that:Equation):Int={
    val a = Equation(_formula.mkString)
    val b = Equation(that.formula.mkString)

    val res = a.evaluate().toDouble - b.evaluate().toDouble

    if(res>0) 1
    else if(res == 0) 0
    else -1
  }

}

object Equation{
  /**@see number a string containging the regular expression of a number
    * */
  val number = """[\+\-]?[0-9]+\.?[0-9]*(?:E\-?[0-9]+)?"""
  /**@see Operation a string containing the regex of common operations and binary expressions eg C and P
    * */
  val operation = """([\+\-\*/CP])"""
  /**@see functions a string containing a regex of common functions
    * */
  val functions = """(log|ln|sin|cos|tan|arc sin|arc cos|arc tan|sec|csc|cot)"""
  /**@see
    *
    */
  val powers = """(\^|"""+'\u23B7'+""")"""
  /**@see unary a regular expression containing unary expressions such as !
    * */
  val unary =(number+"""(!)""").r.unanchored
  /**@see highPrecedence a regular expression containing high precedence operations
    * */
  val highPrecedence = (number+"""([\*/CP])"""+number).r.unanchored
  /**@see lowPrecedence a regular expression containing low precedence operations
    * */
  val lowPrecedence =(number+"""([\+\-])"""+number).r.unanchored
  /**@see arithmeticExpression a regex containing the form of binary operations
    * */
  val arithmeticExpression = ("""""" + number + operation + number +"""""").r.unanchored
  /**@see functionExpression a regex containing the common form of functions
    * */
  val functionExpression = (functions + number).r.unanchored
  /**@see powerOperation
    *
    */

  val powerOperation = (number + powers+number).r.unanchored
  /**@see
    * */
  val stringToTest: Regex = ("""(""" +arithmeticExpression+"""|"""+functionExpression+"""|"""+powerOperation+
    """|"""+unary+""")""").r

  /**Test if the string passed is valid.
    *
    * @todo Improve so it can check the syntax completely
    * @param expression to be error checked
    * @return Whether the string is valid
    * */
  def isValid(expression:String):Boolean={
    expression match {
      case arithmeticExpression(s) => true
      case functionExpression(s)=> true
      case powerOperation(s)=>true
      case unary(s)=>true
      case _ => false
    }
  }


/**
  * Factory method
  *
  * @param formula Expression to be evaluated
  * @return Equation object
  * */
  def apply(formula: String): Equation ={

    new Equation(new StringBuilder(formula))
  }

  /** Utility function that extracts numbers from an expression
    *
    * @param nums
    * @return
    */


  protected def extractNumbers(nums:String): Array[String] = {
    val res = Equation.number.r.unanchored.findAllIn(nums).mkString(" ").split(" ").map {
      (s) => if (s.contains("--")) s.replaceAllLiterally("--", "+") else s
    }

    println("  avnoaen")
    res.foreach(println)
    println("  avnoaen")
    res
  }


  /** Evaluates the string passed in and returns the value
    *
    * @param workingExpr The string to be evaluated, cannot contain any "()"
    * @return The value of the expression as a string
    */

  @scala.annotation.tailrec
  def evaluate(workingExpr:String): String ={

    //println(workingExpr)
    workingExpr match{
      case Equation.powerOperation(s)=>
        val temp = Equation.powerOperation.findFirstIn(workingExpr).get
        evaluate(Equation.powerOperation.replaceFirstIn(workingExpr,evaluatePower(temp)))

      case Equation.functionExpression(s) =>
        val temp = Equation.functionExpression.findFirstIn(workingExpr).get
        evaluate(Equation.functionExpression.replaceFirstIn(workingExpr,evaluateFunction(temp)))

      case Equation.unary(s) =>
        val temp = Equation.unary.findFirstIn(s).get
        evaluate(Equation.unary.replaceFirstIn(workingExpr,evaluateUnary(temp)))

      case Equation.highPrecedence(s) =>
        val temp = Equation.highPrecedence.findFirstIn(workingExpr).get

        evaluate(Equation.highPrecedence.replaceFirstIn(workingExpr,evaluateArithmetic(temp)))

      case Equation.lowPrecedence(s)=>
        val temp = Equation.lowPrecedence.findFirstIn(workingExpr).get
        println(temp)
        evaluate(Equation.lowPrecedence.replaceFirstIn(workingExpr,evaluateArithmetic(temp)))

      case _ =>
        println("No match")
        println(workingExpr)
        if(workingExpr.contains("-")) workingExpr else workingExpr.filter(_!='+')


    }
  }

  /** Utility function used to evaluate Unary expressions
    *
    * @param workingExpr The string to be evaluated
    * @return The value of the expression
    */
 protected def evaluateUnary(workingExpr:String):String={
    val x = Equation.number.r.findFirstIn(workingExpr).get.toLong
    val fact ="""(!)""".r.unanchored
    val res:String =workingExpr match {
      case fact(s) => Operations.factorial(x).toString()
      case _ => workingExpr

    }
    if(res.contains("-")) res else "+"+res
  }

  /**Utility function that evaluates logarithmic and geometric functions
    *
    * @param workingExpr The string to be evaluated
    * @return The value of the expression
    * */

 protected def evaluateFunction(workingExpr:String):String={
    val x = Equation.number.r.unanchored.findFirstIn(workingExpr).get.toDouble
    val logB10 = """(log)""".r.unanchored
    val naturalLog ="""(ln)""".r.unanchored
    val sine ="""(sin)""".r.unanchored
    val cosine = """(cos)""".r.unanchored
    val tangent = """(tan)""".r.unanchored
    val arcSine ="""(arc sin)""".r.unanchored
    val arcCosine = """(arc cos)""".r.unanchored
    val arcTangent ="""(arc tan)""".r.unanchored
    val secant = """(sec)""".r.unanchored
    val coSecant = """(csc)""".r.unanchored
    val coTangent ="""(cot)""".r.unanchored

    val res:String =workingExpr match {
      case logB10(s) => log10(x).toString
      case naturalLog(s) => log(x).toString
      case arcSine(s)=> asin(x).toString
      case arcCosine(s)=> acos(x).toString
      case arcTangent(s)=> atan(x).toString
      case sine(s)=> sin(x).toString
      case cosine(s)=> cos(x).toString
      case tangent(s)=> tan(x).toString
      case secant(s)=>(1/cos(x)).toString
      case coSecant(s)=>(1/sin(x)).toString
      case coTangent(s)=>(1/tan(x)).toString
      case _=> workingExpr
    }
    if(res.contains("-")) res else "+"+res

  }

  /**
    * Utility function that evaluates binary operations
    *
    * @param workingExpr Expression to be evaluated
    * @return The value of the expression
    */
 protected def evaluateArithmetic(workingExpr:String):String={

    val num: Array[String] = Equation.extractNumbers(workingExpr)
    val addition: UnanchoredRegex ="""(\+)""".r.unanchored
    val subtraction: UnanchoredRegex ="""(\-)""".r.unanchored
    val divide: UnanchoredRegex = """(/)""".r.unanchored
    val multiply: UnanchoredRegex ="""(\*)""".r.unanchored
    val combination: UnanchoredRegex = """(C)""".r.unanchored
    val permutation: UnanchoredRegex ="""(P)""".r.unanchored

    val res:String =workingExpr match {
      case combination(s) => Operations.C(num(0).toInt, num(1).toInt).toString()
      case permutation(s) => Operations.P(num(0).toInt,num(1).toInt).toString()
      case multiply(s)=> (BigDecimal(num(0))*BigDecimal(num(1))).toString()
      case divide(s)=> (BigDecimal(num(0))/BigDecimal(num(1))).toString()
      case addition(s)=> (BigDecimal(num(0))+BigDecimal(num(1))).toString()
      case subtraction(s)=> (BigDecimal(num(0))+BigDecimal(num(1))).toString()

      case _=>workingExpr
    }
    if(res.contains("-")) res else "+"+res

  }

  /**
    * Utility function that evaluates powers
    *
    * @param workingExpr Expression to be evaluated
    * @return The value of the expression
    */
 protected def evaluatePower(workingExpr:String):String={
    val num: Array[String] = Equation.extractNumbers(workingExpr)
    val radix: UnanchoredRegex ="(\u23B7)".r.unanchored
    val caret: UnanchoredRegex ="""(\^)""".r.unanchored

    val res:String =workingExpr match {

      case caret(s) =>
        pow(num(0).toDouble, num(1).toDouble).toString

      case radix(s) =>
        pow(num(1).toDouble,1/num(0).toDouble).toString

      case _=>workingExpr
    }
    if(res.contains("-")) res else "+"+res

  }

  def parseExponent(formula:StringBuilder):StringBuilder  ={
    if(formula.contains('E')){
      val index = formula.indexOf('E')
      formula.replace(index,index+1,"*10^")

    }else formula
  }


}

