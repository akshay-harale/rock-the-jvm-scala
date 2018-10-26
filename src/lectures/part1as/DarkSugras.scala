package lectures.part1as

import scala.util.Try

object DarkSugars extends App{
  //1: methods with single param

  def singleArgumentMethod(i:Int):String = s"$i little ducsk ";

  val descciption= singleArgumentMethod{
    // write some code
    42
  }

  val aTryInstace= Try{ // javas try{}
    throw  new RuntimeException

  }

  List(1,2,3).map{
    x=> x+1
  }

  //2: single abstract method pattern

  trait Action {
    def act(i:Int):Int
  }

  val anActionInstance= new Action {
    override def act(i: Int): Int = ???
  }

  val anFunkyActionInstance= (x:Int)=> x+1 //magic
  //example Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello scala in thread")
  })

  val funckyThread= new Thread(()=>println("this is scary"))

  abstract class AbstractType{
    def implemented:Int = 2
    def f(i:Int):Unit
  }

  val abstractInstance:AbstractType= (a:Int)=>println("something looks like implicites but not")

  //3:the :: and #:: methods are special
  val prependedList= 2 :: List(3,4) //there is is not 2.:: on int
  //but there is List().:: operator on List()
  //scala spec: last character decides associativity of method

  class MyStream[T] {
    def -->:(value:T):MyStream[T] = this //implementation
  }

  val myStream= 1 -->: 2-->: 3 -->: new MyStream[Int] //


  //4: multi word method naming
  class TeenGirl(name:String){
    def `then she said`(message:String):String=s"$name said $message"
  }

  val lilly= new TeenGirl("lilly")
  lilly `then she said` "scala is awesome"

  //5: infix types
  class -->[A,B]

  val infix: Int --> String = ???

  //6: update() much like apply()
  val anArray= Array(2,3,4)
  anArray(2) = 7 // rewritten to anArray.update(2,7)

  //7: setters for mutable containers
  class Mutable{
    private var internalsMember:Int =0

    def member:Int = internalsMember

    def member_=(value:Int):Unit =
      internalsMember= value
  }
  var mutableContainer= new Mutable()
  mutableContainer.member = 42  //converted to mutableContainer.member_=(42 )




}
