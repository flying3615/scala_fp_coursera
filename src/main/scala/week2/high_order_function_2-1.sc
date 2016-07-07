def sumInt(a:Int,b:Int):Int =
  if(a>b) 0 else a+sumInt(a+1,b)

def cube(x:Int):Int = x*x*x

def sumCube(a:Int,b:Int):Int =
  if(a>b) 0 else cube(a)+sumCube(a+1,b)


//summing with high-order functions

def sum(f:Int=>Int,a:Int,b:Int):Int =
  if(a>b) 0 else f(a)+sum(f,a+1,b)

//then we can re-write upper functions like
def id(x:Int) = x
def sumInts(a:Int,b:Int) = sum(id,a,b)

def factorials(x:Int):Int = if(x==0) 1 else factorials(x-1)
def sumFactorial(a:Int,b:Int):Int = sum(factorials,a,b)

def sumCubes(a:Int,b:Int) = sum(cube,a,b)

//A=>B is function type that takes a parameter of type A and return type B
//so Int=>Int is the function that map integer to integer

//anonymous function, don't have a name,
// the type of parameter can be omitted if the type can be inferred from the context
(x:Int)=>x*x*x
(x:Int,y:Int)=>x+y
//using anonymous function
def sumInts_anony(a:Int,b:Int):Int = sum(x=>x,a,b)
def sumCubes_anony(a:Int,b:Int):Int = sum(x=>x*x*x,a,b)


//The sum function of tail-recursive version
def sum_exercise(f:Int=>Int,a:Int,b:Int):Int = {
  def loop(a:Int,acc:Int):Int = {
    if(a>b) acc
    else loop(a+1,f(a)+acc)
  }
  loop(a,0)
}

sum_exercise(x=>x*x,3,5)


