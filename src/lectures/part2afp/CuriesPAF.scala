package lectures.part2afp

object CuriesPAF extends App{

  //curried functions
  val superAdder:Int=>Int=>Int =
    x => y => x + y

  val add3 = superAdder(3) //Int => Int = y => 3 + y
  println(add3(5))
  println(superAdder(5)(3)) //curried function
  def curriedAdder(x:Int)(y:Int):Int = x+y

  val add4 :Int => Int = curriedAdder(4) // curried method
  println(add4(3))
  // lifting = ETA - EXPANSION
  def inc(x:Int)=x+1

  List(1,2,3).map(x=>inc(x))

  // Partial Function applications

  val add5 = curriedAdder(4) _ //function arity

  //EXERCISE
  val simpleAddFunction = (x:Int,y:Int)=> x+y
  def simpleAddMethod(x:Int,y:Int)=x+y
  def curriedAddMethod(x:Int)(y:Int)=x+y

  val add7 = (x:Int)=>simpleAddFunction(7,x)

  val add7_2 = simpleAddFunction.curried(7)


  val add7_3 = curriedAddMethod(7) _
  val add7_4 = curriedAddMethod(7)(_)

  val add7_5 = simpleAddFunction(7,_:Int) // force compiler to do ETA expansion
  val add7_6 = simpleAddMethod(7,_:Int) //convert method into function values

  //underscores are powerful
  def concatenator(a:String,b:String,c:String)=a+b+c
  val insertName = concatenator("Hi!I am ",_:String,", how are you") // x:String => concatenator(hello,x,something)
  println(insertName("raju"))

  //Exercise
  /*
    1. Process a list of members and return their string representation with different formats.
       Use the %4.2f , %8.6f and %14.12f with curried formatter function.
   */
  val doubles = List(Math.PI,Math.E)
  def curriedFormatter  (s:String)(number:Double):String = s.format(number)

  val simpleFormat=curriedFormatter("%4.2f") _  //lift convert the two param method into one param
  val seriousFormat = curriedFormatter("%8.6f") _ //
  val preciseFormat = curriedFormatter("%14.12f") _

  println(doubles.map(preciseFormat))
  println(doubles.map(curriedFormatter("%4.2f"))) //compiler does the ETA expansion for us


  /*
    2. difference between
      - functions vs methods
      - parameters: byname vs 0-lambda
   */

  def byName (n: => Int) = n+1
  def byFunction(f:() => Int ) = f()+1

  def method:Int = 42
  def parenMethod():Int = 42

  byName(1)
  println(byName(method))
  println(byName(parenMethod()))
  //byName(()=>42) //not OK
  byName((() => 42)())
  //byName(parenMethod _ )// will not work

  //byFunction(42)// will not work as it expects function not value as param
  println(byFunction(method _)) // will do ETA expansion we are senfin PAF
  //println(byFunction(method)) // will not do ETA expansion
  println(byFunction(parenMethod)) //will do ETA expansion we are sending PAF
  byFunction(()=>42) // accepting function so this will work


}
