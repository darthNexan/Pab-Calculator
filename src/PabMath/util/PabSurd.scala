
package PabMath.util

import scala.math.pow
/**
  * Created by Dennis on 7/22/2016.
  */
class PabSurd(_y:Int,_i:Int, _x:Int) {

  override def toString ={
    val y:String =  if(this._y>1) this._y.toString+"*" else ""
    val i:String =  if(this._i>2) this._i.toString else ""

    y+ i + '\u23B7' + _x
  }
  def this(_i:Int, _x:Int) = this(1,_i,_x)

  val y = this._y
  val i = this._i
  val x = this._x

}

object PabSurd{

  def apply(num :Float):PabSurd = {
    require(num!=0)
    def compute(num:Float) :(Int,Int,Int)  = {
      val range: Int = if (num<2) 64 else if (num<10) 18 else if(num<50)  11 else if(num<100) 9 else 4
      def findI(num:Float,range: Int, count:Int):(Int,Int)={

        val res = pow(num, count)
        if(( res*0.9999 <= res.toLong )&&(res *1.0001 >= res.toLong))
        (count,res.toInt )//indice,radicand
        else if(range == count) (0,0)
        else findI(num, range,count+1)
      }

      def findY(rawSurd:(Int,Int)):(Int,Int,Int) ={
        if(rawSurd == (0,0)) return (0,0,0)
        val range= 2D to pow(rawSurd._1.toDouble,1/rawSurd._2.toDouble) by 1
        val radicand = rawSurd._2

        for(x<-range if radicand%pow(x,rawSurd._2) == 0) return (pow(x,1/rawSurd._1).toInt,rawSurd._1 ,rawSurd._2/x.toInt)

        (1,rawSurd._1, rawSurd._2)


      }

      findY(findI(num,range,2))
    }


      val res = compute(num)
      new PabSurd(res._1,res._2,res._3)




  }

}