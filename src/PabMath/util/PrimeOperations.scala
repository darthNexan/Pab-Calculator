package PabMath.util

import java.io.{File, FileNotFoundException, PrintWriter}
import java.nio.file.FileSystemNotFoundException

import scala.collection.SortedSet
import scala.io.Source
import scala.math._

/**
  * Created by Dennis on 7/22/2016.
  */
object PrimeOperations {

//
//  /**Loads a sorted set of Prime numbers from 1 to 10M
//    * @return A sorted set of prime numbers
//    * */
//  private def initialize(): Set[Int]={
//
//    val br = Source.fromFile("Primes1to10M.txt").bufferedReader()
//    var line:String =""
//    var sieve:Set[Int] = Set()
//
//    try {
//      while ( {
//        line = br.readLine(); line != null
//      }) sieve +=line.toInt
//
//      sieve
//    } finally{
//      br.close()
//    }
//  }
//
//  /**@see Sieve to be used to test large numbers >100B
//    * */
//  private val sieve:Set[Int]= {
//    try{
//    initialize()
//    }catch{
//      case e: FileNotFoundException =>{
//        var pw = new PrintWriter(new File("Primes1to10M.txt"))
//        val temp = for(x<-1 to 10000000 if isPrime(x)) yield x
//
//        try{
//          temp.foreach(pw.println)
//        }
//        finally{
//          pw.close
//        }
//        temp.toSet
//      }
//    }
//  }
//  /**@see Sieve used to test smaller primes <100B
//    **/
  private val simpleSieve:SortedSet[Int] = SortedSet(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97)

  /**genPrimes(min, max)
    * Used to generate a Seq of prime numbers. Why would you want that? Idk I am just writing the library kk?
    * @param min the first number to be tested, preferably should be odd
    * @param max the last number to be tested
    * @return Seq[Long]
    * */
  def genPrimes(min:Long, max: Long):Seq[Long] ={

    val rangeMin=if(min%2==0) min+1 else min
      for (x <- rangeMin to max by 2 if isPrime(x))yield x

  }
//
//  /**isPrimeSieve(num)
//    * Used to check if a large number > 10B is a prime.
//    * @throws IllegalArgumentException if 0 is passed as an arg
//    * @param num the value to be tested
//    * @return Boolean
//    * */
//  def isPrimeSieve(num:Long):Boolean={
//    if(num==0) throw new IllegalArgumentException("isPrimeSieve: 0 is not a valid argument")
//    else if((num ==1)||((num>2)&&(num%2==0))) false
//    else sieve.filter(_<sqrt(num.toDouble)).exists(num%_==0)
//
//  }
//  /**primeFactorize(num)
//    * Used to generate a sorted set of Tuple pairs where _1 is the prime and _2 is the power. The counter is recursive.
//    * @param num The number that is to be factorized
//    * @return SortedSet[(Int,Int)]
//    *
//    * */
//
//  def primeFactorize(num:Long):Set[(Int, Int)] ={
//    def counter(factor:Int,num:Long):Int = if (num%factor!=0) 0 else 1+counter(factor, num/factor)
//
//    for (factor <- sieve.filter((x:Int)=> num %x == 0 && num/2>=x ) ) yield {
//
//      Tuple2(factor, counter(factor, num))
//    }
//  }


  /**isPrime(num)
    * Used to determine if a num smaller than 10B is prime.
    * @throws IllegalArgumentException if 0 is passed as an argument
    * @param num the number to be tested
    * @return Boolean
    * */
  def isPrime(num:Long):Boolean ={
    require(num!=0,"isPrime: 0 is not a valid argument")
    
    if((num==1)||simpleSieve.filter(_<sqrt(num)).exists(num%_ == 0 ) ) false
    else if(sqrt(num)<=97) true
    else {
      val range:Range = 101 to (sqrt(num.toDouble).toInt + 1) by 2
      !range.exists(num%_==0)

    }
  }
}
