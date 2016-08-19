package PabMath.util

/**
  * Created by dguye on 7/17/16.
  */


import scala.BigInt
import scala.collection.SortedSet
import scala.math._


//  TODO: 1. Generate a list of known primes that can be used to check against rather than checking all values
//  TODO: 2. Write a function to be performed on creation of PabMath
//  TODO: 3. Optimize fibonacci to use scala tail call method

object Operations {

  /** factorize
    * Generates a sorted set containing all the factors of num by searching for factors from num to sqrt(num)
    *
    * @param num The number to be factorized
    * @return A sorted set of integer factors
    * */
  def factorize(num:Int):SortedSet[Int] = {

    var factors = SortedSet(1, num)

    for(x<- 2 to sqrt(num.toDouble).toInt if num%x == 0) factors+=x

    factors
  }

  /** factorial
    * Generates x!
    *
    * @throws IllegalArgumentException if x<=0
    * @param n of type BigInt
    * @return The factorial of x
    * */

  def factorial(n:BigInt): BigInt = {
    require(n>0)
    (BigInt(1)/:(BigInt(1) to n).toList)(_*_)
  }
  /** nCr
    * Calculates the  value of n combination r
    *
    * @param n a non zero integer required to be larger than r
    * @param r a non zero integer
    * @throws IllegalArgumentException if n < r or r ==0 or n==0
    * */
  def C(n:Int,r:Int): BigInt ={
    require(!((n<=0)||(r<=0)||(r>n)),"C(n,r): Math Error!")

    factorial(n)/(factorial(r)*factorial(n-r))

  }
  //nCr func

  /**nPr
    * Calculates the value of n permutation r
    *
    * @param n a non zero integer required to be larger than r
    * @param r a non zero integer
    * @throws IllegalArgumentException if n < r or r == 0  or n == 0
    * */
  def P(n:Int,r:Int): BigInt={
    require(!((n<=0)||(r<=0)||(r>n)),"P(n,r): Math Error!")
    factorial(n)/factorial(n-r)
  }
  //nPr func

  /**fib
    * Calculates the nth term in the fibonacci series.
    * @param n the term to be calculated
    * @return the value of the term
    *
    * */
  def fib(n:Long): Long = {
    require(n > 0, "fib(n): Math Error!")

    if ((n == 0) || (n == 1))
      1
    else
      fib(n - 1) + fib(n - 2)
  }
  //fibonacci's sequence


  //returns an ordered set containing the common factors of x and y
  /**commonFactors
    * Finds the intersect between the factors of x and y
    * @param x the smaller of the two numbers
    * @param y the larger of the two numbers
    * @return the intersect of the factors
    * */
  def commonFactors(x:Int, y:Int) = factorize(x).filter( y%_==0)

  /**HCF
    * Finds the highest common factor using the Euclidean algorithm
    * @param x the first num
    * @param y the secon num
    * @return The highest common factor
    * @throws IllegalArgumentException if either input is 0
    * */

  def HCF(x:Int, y:Int):Int =  if(y==0) x else HCF(y,x%y)


  /** arithmeticProgression
    * Generates a sequence of numbers representing the arithmetic sequence from a to an
    * @param n the number of terms to generate
    * @param a the first term in the sequence
    * @param d the common difference between subsequent terms
    * @return the sequence of terms
    * @throws IllegalArgumentException If n is less than or equal to zero. Also it will not throw an exception but do
    *                                  not make d equal to zero that is just stupid and you should feel bad.
    * */
  def arithmeticProgression(n:Int, a:Double, d:Double): Seq[Double] = {
    require(n>0, "Math error: n cannot be equal or less than zero")
    for (x <- 1 to n) yield a + (x - 1) * d
  }

  /** geometricProgression
    * Generates a sequence of terms representing the geometric progression from a to an
    * @param n the number of terms to generate
    * @param a the first term in the sequence
    * @param r the common factor
    * @return the sequence of terms
    * @throws IllegalArgumentException If n less than or equal to 0 or if a or r is equal to zero
    * */
  def geometricProgression(n:Int, a:Double, r:Double): Seq[Double] = {
    require(a!=0 || r!=0 || n>0, "Math error: a and r cannot be equal to 0 and n must be greater than 0")
    for(x<-1 to n) yield a*pow(r,(x-1).toDouble)
  }


  /**fibonacciSeries
    * Generates fibonacciSeries of length n
    * @param n the number of terms to generate
    * @return the fibonacci series up to the nth term
    * */
  def fibonacciSeries(n:Long) = {
    require(n>=0,"Math Error, n cannot be negative" )
    for (x <- 0L to n) yield fib(x)
  }

  /**sumGeometric
    * Generates the sum of the terms from a to an
    * @param n the last term to consider
    * @param a the first term in the sequence
    * @param r the common difference between subsequent term
    * @throws IllegalArgumentException If n less than or equal to 0 or if a or r is equal to zero
    * */
  def sumGeometric(n:Int, a:Double, r:Double) = {
    require(a!=0 || r!=0 || n>0, "Math error: a and r cannot be equal to 0 and n must be greater than 0")
    a * (1 - pow(r, n)) / (1 - r)
  }

  /** sumArithmetic
    * Generates the sum of the terms from a to an
    * @param n the last term to consider
    * @param a the first term in the sequence
    * @param d the common difference between subsequent terms
    * */
  def sumArithmetic(n:Int, a:Double, d:Double) = {
    require(n>0, "Math error: n cannot be equal or less than zero")
    (n / 2) * (2 * a + (n - 1) * d)
  }

  def toRad(angle:Double):Double ={
    (angle/180)*Pi
  }
  def toDeg(angle:Double):Double ={
    (angle*180)/Pi
  }


}
