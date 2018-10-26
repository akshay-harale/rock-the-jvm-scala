package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {

  val aCondition : Boolean = false
  val aConditionVal = if(aCondition) 42 else 65
  //instructions vs expression

  //compiler infers types for us
  val aCodeBlock = {
    if(aCondition) 56
    65
  }

  //Unit is doing only side effect
  val theUnit = println("hello scala")

  //functions
  def aFunction(i:Int,j:Int):Int = i+j

  //recursion

  @tailrec def factorial(n:Int,accumulator: Int):Int = {
    if(n<=0)
      accumulator
    else
      factorial(n-1,n*accumulator)
  }

  def factorialOld(n:Int):Int = {
    if(n>=1)
      n*factorialOld(n-1)
    else
      1
  }

  println(factorial(3,1))

  println(factorialOld(3))

  // OOP

  class Animal
  class Dog extends  Animal
  val aDog:Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(a:Animal):Unit
  }

  class Crocodile extends Animal with Carnivore{
    override def eat(a: Animal): Unit = println("crunch")
  }

  //method notation

  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog //natural language

  //anonymous classes
  val carnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  //generics

  abstract class MyList[+A] //variance and its types
  //singleton and companions
  object MyList

  //case classes
  case class Person(name:String,age:Int)

  //exceptions and try/catch/finally
  val throwableException = throw new RuntimeException//Nothing

  val aPotentialFailure = try{
    throw new RuntimeException
  } catch {
    case e:Exception => "I got exception"
  } finally {
    println("some logs")
  }
  //imports and packaging


  //functional programming : functions are actually instances with apply method.
  // Everything in scala in classes and objects
  val increamenter = new Function1[Int,Int] {
    override def apply(v1: Int): Int = v1+1
  }
  increamenter(1)

  val annoIncreamenter= (x:Int)=> x+1

  List(1,2,3).map(annoIncreamenter) //map is HOF

  //for-comprehensions are the syntactic sugar for the map flatMap and filters
  val pairs = for{
    num <- List(1,2,3) //we can put if condition
    chars <- List("a","b","c")
  } yield num +"-"+chars

  //scala collections: Seqs, Arrays, Lists,Vectors,Maps,Tuples
  val aMap=Map(
    "Akshay" -> 123,
    "Rahul" -> 234,
    "Swaps" -> 345
  )

  //"collection":Option and Try
  val anOption = Some(2)

  //pattern matching
  val x=2
  val order = x match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case _ => "anythinf"
  }

  val bob = Person("Bob",22)
  val greeting= bob match {
    case Person(name,_)=>s"Hi my name is $name"
  }
  // all patterns
}
