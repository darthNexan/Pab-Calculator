package PabTools

/**
  * Created by dennis on 7/31/16.
  */
object StringManipulations {

  /**
    * Extracts the operand and performs an operation on the contents of the string. If the operand contains multiple
    * instances of the delimiters the last instance will be evaluated and the contents will be returned
    * @todo Allow use of identical delimiters
    * @param operand String to be operated on
    * @param delimiterBegin Beginning of the substring
    * @param delimiterEnd End of the substring
    * @param op Operation to be performed on the substring
    * @throws IllegalArgumentException Thrown if the string does not contain both delimiters
    * @throws StringIndexOutOfBoundsException Thrown if the delimiters are not in the correct order.
    * @return
    */
  def extract(operand:String,delimiterBegin:Char, delimiterEnd:Char)(op:(String)=>String): String ={
    require(operand.contains(delimiterBegin)&&operand.contains(delimiterEnd))

    def start = operand.lastIndexOf(delimiterBegin)
    def end = operand.indexOf(delimiterEnd,start+1)

    op(operand.substring(start, end).filter((c: Char) => c != delimiterBegin && c != delimiterEnd))

  }

  /**Extracts and replaces the last instance of the string between the delimiters and performs the operation.
    *
    * @param operand
    * @param delimiterBegin
    * @param delimiterEnd
    * @param op
    * @throws IllegalArgumentException
    * @throws IllegalArgumentException
    */
  def extractReplace(operand:StringBuilder,delimiterBegin:Char,delimiterEnd:Char)(op:(String)=>String): Unit ={

    def start = operand.lastIndexOf(delimiterBegin)
    def end = operand.indexOf(delimiterEnd,start+1)
    operand.replace(start,end,extract(operand.mkString,delimiterBegin,delimiterEnd)(op))

  }

  /** Finds last matching instance of the regular expression.
    *
    * @param s
    * @param regex
    * @return (String,int) the last matching instance of the regex and the index where it starts
    */
  def findLastMatch(s:String,regex:String):(String,Int)={

      
      val res1 = regex.r.unanchored.findAllIn(s).toList.last
      (res1, s.lastIndexOfSlice(res1))


  }
}
