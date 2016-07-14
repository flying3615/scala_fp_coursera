val x = new Rational(1,3)
x.numer
x.denom
val y = new Rational(5,7)
val z = new Rational(3,2)


x + y

x - y - z


x < y

x.max(y)

val strange = new Rational(0,1)

strange + strange


class Rational(x:Int,y:Int){

  require(y!=0,"denomation cannot be zero")

  def numer = x
  def denom = y

  def this(x:Int) = this(x,1)


  private def gcd(a:Int,b:Int):Int = {
    if(b==0) a else gcd(b,a%b)
  }


  def + (that:Rational)={
    new Rational(
      numer*that.denom+that.numer*denom,
      denom*that.denom)
  }


  def < (that:Rational) = numer*that.denom<that.numer*denom

  def max(that:Rational) = if(this < that) that else this

  override def toString = {
    val g = gcd(x,y)
    numer/g+"/"+denom/g
  }

//  def mul(that:Rational)={
//    new Rational()
//  }

  def unary_- = new Rational(-numer,denom)

  def - (that:Rational) = {
    this + -that
  }

}