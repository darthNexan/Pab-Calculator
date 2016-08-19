package PabMath.util

/**
  * Created by dguye on 7/18/16.
  */

/**@constructor Use the apply method supplied instead
  *            @param _n The numberator of the fraction
  *            @param _d The denominator of the fraction
  *
  * */
class PabFraction(_n:Int, _d:Int) {
  require(_d!=0,"Math Error: Fractions are undefined for d<0")

  def this(x:Int)={
    this(x,1)
  }



  val n:Int = this._n
  val d:Int = if(n==0) 1 else this._d


  override def toString = new String(_n+"""/"""+_d)


  def +(addend:PabFraction):PabFraction ={
    val res = new PabFraction(
      (this.n*addend.d) + (this.d * addend.n) ,
      this.d * addend.d
    )

    PabFraction.simplify(res)
  }

  def -(subtrahend:PabFraction)={

    val res = new PabFraction(
      (this.n*subtrahend.d) - (this.d * subtrahend.n) ,
      this.d * subtrahend.d
    )

    PabFraction.simplify(res)
  }

  def *(multiplicand:PabFraction) ={

    val res = new PabFraction(
      this.n * multiplicand.n,
      this.d * multiplicand.d
    )
    PabFraction.simplify(res)
  }

  def /(divisor:PabFraction)  ={

    if(divisor.n == 0)
      throw new IllegalArgumentException("Math Error: result undefined for x/divisor")

    else {
      val res = new PabFraction(
        this.n * divisor.d,
        this.d * divisor.n
      )
      PabFraction.simplify(res)
    }
  }

  def == (x:PabFraction) =
    x.d*this.n == x.n *this.d

  def >(x:PabFraction) =
    x.d*this.n> x.n*this.d

  def >=(x:PabFraction) =
    x.d*this.n >= x.n*this.d

  def <(x:PabFraction) =
    x.d*this.n < x.n*this.d

  def <=(x:PabFraction) =
    x.d*this.n <= x.n*this.d

  def !=(x:PabFraction) =
    x.d*this.n != x.n*this.d


  def toFloat =
    this.n.toFloat / this.d.toFloat

  def toInt =
    this.n / this.d


}

object PabFraction{


  /** Simplifies fractions by producing a set containing the common factors of n and d if the only factor is 1 it returns the fraction
    *         else it reduces n and d and recursively calls itself.
    *
    *  @param fraction The fraction that is going to be simplified
    *  @return simplifiedFraction The fraction that has been simplified
    * */
  def simplify(fraction: PabFraction): PabFraction ={
    if(fraction.n ==0){
      new PabFraction(0,1)
    }
    else{
      val factor = Operations.HCF(fraction.n, fraction.d)
      new PabFraction(fraction.n / factor,fraction.d / factor)
    }
  }
  /** This is a factory method that produces a fraction object from a float. This is done by multiplying
    * x by a power of ten and checking for truncation. This is repeated until the operation succeeds with no
    * truncation.
    *
    * @param x The float that is to be converted to a fraction.
    * @return fraction that has already been simplified
    * */
  def apply(x:Float): PabFraction ={
    def converter(x:Float, coefficient:Int):PabFraction =
      if (x*coefficient == (x*coefficient).toInt )
        new PabFraction((x*coefficient).toInt, coefficient)
      else
        converter(x, coefficient*10)

    simplify(converter(x,10))

  }

  def apply(_n: Int, _d: Int): PabFraction = simplify(new PabFraction(_n, _d))

  def apply(_x:Int): PabFraction = new PabFraction(_x)

}