//Sum is now a function that returns another function
def sum(f:Int=>Int):(Int,Int)=>Int = {
  def sumF(a:Int,b:Int):Int =
    if(a>b) 0
    else f(a)+sumF(a+1,b)
  sumF
}

//rewrite sum with syntax sugar
//writing in this style is aiming for being able to
//write like sum_rewrite(code) and don't need to worry about the
//other two parameters
def sum_rewrite(f:Int=>Int)(a:Int,b:Int):Int =
  if(a>b) 0 else f(a) + sum(f)(a+1,b)


//Exercise

def mapReduce(f:Int=>Int,combine:(Int,Int)=>Int,zero:Int)(a:Int,b:Int):Int=
  if(a>b) zero
  else combine(f(a),mapReduce(f,combine,zero)(a+1,b))

def product(f:Int=>Int)(a:Int,b:Int):Int = mapReduce(f,(x,y)=>x*y,1)(a,b)

product(x=>x*x)(3,4)

def fact(n:Int) = product(x=>x)(1,n)

fact(5)


