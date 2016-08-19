import PabMath.util.PrimeOperations

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import PabMath.util.{Operations, PrimeOperations}

import scala.BigDecimal
import scala.math._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

val number = """[\-\+]?[0-9]+\.?[0-9]*"""
val operation = """([\+\-*/CP])"""
val functions = """(log|ln|sin|cos|tan|arc sin|arc cos|arc tan|sec|csc|cot)"""
val powers = """(\^)"""+number
val unaryOperation: Regex =(number+"""(!)""").r.unanchored




val arithmeticExpression = (""""""+number + operation + number+"""""").r.unanchored
val functionExpression = (functions + number).r.unanchored
val powerOperation = (number + powers).r.unanchored
val stringToTest: Regex = ("""(""" +arithmeticExpression+"""|"""+functionExpression+"""|"""+powerOperation+""")""").r

println(powerOperation.findFirstMatchIn("10^4").get)
val expression = powerOperation.findFirstIn("10^4")

private def evaluateUnary(workingExpr:String):String={
  val x = number.r.findFirstIn(workingExpr).get.toLong
  val fact ="""(!)""".r.unanchored
  workingExpr match {
    case fact(_) => Operations.factorial(x).toString()
    case _ => workingExpr

  }
}

evaluateUnary("5!")


val expr1:StringBuilder = new StringBuilder("x+y")
val b = Map("x"->"5","y"->"7")
val gh = expr1.indexOf("x")

expr1.replace(gh,gh+1,"5")

println(expr1.mkString)


val a =  BigDecimal("4E7") + BigDecimal("7")

5E-19

println("This is another test")


def extract(operand:String,delimiterBegin:Char, delimiterEnd:Char)(op:(String)=>String): String ={
  require(operand.contains(delimiterBegin)&&operand.contains(delimiterEnd))

  def start = if(delimiterBegin!=delimiterEnd) operand.lastIndexOf(delimiterBegin) else operand.indexOf(delimiterBegin)
  def end = operand.indexOf(delimiterEnd,start+1)

  op(operand.substring(start, end).filter((c: Char) => c != delimiterBegin && c != delimiterEnd))

}

extract(";Some texy;",';',';'){
  (s:String)=> println(s)
      s
}


def append[T](list1:List[T],list2:List[T]):List[T] ={
  list1 match{
    case List() => list2
    case x::xs => x::append[T](xs,list2)
  }
}

val list1 = 1::2::3::Nil
val list2 = 1::2::Nil

append[Int](list1,list2)

def length(list: List[Any]): Int ={
  list match {
    case List()=> 0
    case x::xs => 1+length(xs)
  }
}

length(append[Int](list1,list2))


def name(FName:String ="Paul",LName:String)= s"$FName $LName"

name("John","Doe")
name(LName = "Doe")


lazy val rob = {println("Hello");name("John","Doe")}


println(s"Hello $rob")


var writer = new BufferedWriter(new FileWriter("/home/dennis/Test1.txt",true))

writer.append("Hello from scala\n")
writer.append("Hello can you hear me\n")
writer.close()

def findLastMatch(s:String,regex:String)={
  val res1= regex.r.unanchored.findAllIn(s).toList.last
  (res1,s.lastIndexOfSlice(res1))
}




findLastMatch("Num 1234 Num 1234 Num 12834","Num [1-9]+")

val s ="Num 1234 Num 1234 Num 12834"

if(s.charAt(18)==' ') println("Space")
s.charAt(18).toString

findLastMatch("4+5--1","""[\-\+]?[0-9]+\.?[0-9]*(?:E\-?[0-9]*)?""")

def fact(n:Int) = List.range(1, n+1).product

fact(100)

def fact1(n:BigInt) = {
  require(n>0)
  (BigInt(1)/:(BigInt(1) to n).toList)(_*_)
}

fact1(100)

val str= new StringBuilder("Hello World")

str.deleteCharAt(4)
str.insert(4,'f')

println(str.mkString)






val trigFunctions:List[(String, String, String)] = List(("sin","arc sin","csc"),("cos","arc cos","sec"),("tan","arc tan","cot"))

val (mode1,mode2,mode3) = trigFunctions.unzip3

mode1.foreach(println)


var pw = new PrintWriter(new File("Primes1To10M.txt"))
PrimeOperations.genPrimes(1,10000000).foreach(pw.println)
