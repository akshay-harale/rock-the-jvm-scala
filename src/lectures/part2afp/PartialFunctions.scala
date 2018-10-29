package lectures.part2afp

object PartialFunctions extends App{

  val aFunction = (x:Int) => x+1 //Function1[Int,Int]

  val aFussyFunction = (x:Int) =>{
    if (x == 1) 42
    if (x == 2) 56
    if (x == 5) 999
    else throw new FunctionNotApplicableException
  }

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFunction = (x:Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }


  val aPartialFunction :PartialFunction[Int,Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } //partial function value

  println(aPartialFunction(2))
  //println(aPartialFunction(34))

  //PF utilities

  println(aPartialFunction.isDefinedAt(34))

  //lift
  val lifted = aPartialFunction.lift
  println(lifted(2))
  println(lifted(23))

  val pfChain=aPartialFunction.orElse[Int,Int] {
    case 23 => 23
  }

  println(pfChain(2))
  println(pfChain(23))

  //PF extends normal function
  val aTotalFunction: Int=>Int={
    case 4=>4
  }

  val aMappedList=List(1,2,3).map{
    case 1 => 42
    case 2 => 56
    case 3 => 999
  }

  println(aMappedList)

  /*
  Note PF can only have one parameter type. Because of pattern matching.
   */

  /**
    * Exercises:
    * 1. Construct a PF instance yourself (anonymous class)
    * 2. Chatbot using FP
    */

  val aManualFussyFunction: PartialFunction[Int, Int] = new PartialFunction[Int,Int] {
    override def isDefinedAt(x: Int): Boolean =
      x==1 || x == 2 || x == 5

    override def apply(x: Int): Int = x match {
      case 1 => 42
      case 2 => 56
      case 5 => 999
    }

//    println(aManualFussyFunction(1))

    val chatbot = (x:String) => x match {
      case "Hello"=>"Hi! How are you?"
      case "goodbye"=> "No way"
    }

    scala.io.Source.stdin.getLines().map(chatbot).foreach(println)

  }



}
