package lectures.part1as

object AdvancedPatterMatching extends App{

  val numbers= List(1)
  val description= numbers match {
    case head :: Nil => println(s"only element is $head")
    case _ => println("multliple valules")
  }

  /*
    We can use following things in scala pattern matching
    - constants
    - wildcards
    - case classes
    - tuples
    - some special things like above
   */

  class Person(val name:String, val age:Int) //lets use this in pattern matching.
  // We  will write some code to make use of class as like case class in pattern matching
  
  object Person{
    def unapply(person: Person): Option[(String, Int)] =
        Some(person.name,person.age)

    def unapply(age: Int): Option[String] =
      Some(if(age<21) "minor" else "major")
  }

  val bob = new Person("bob",25)
  val greeting = bob match { //here bob object is being passed so it will call the unapply with Person
    case Person(a,b)=>s"Hi, my name is $a and I am $b y old" //pattern matching class name is actually a companion object name

  }
  println(greeting)

  val legalStatus = bob.age match { //because age is being passed to the pattern matching it will call the unapply with int
    case Person(status) => s"my legal status is $status"
  }
  println(legalStatus)

  /*
  Exercise
   */
  val n:Int = 2

  object even {
    def unapply(arg: Int): Boolean = arg %2 ==0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg <10
  }

  val mathProperty = n match {
    case singleDigit() => s"is single digit"
    case even()=> s"is even"
    case _ => "don't know"
  }
  println(mathProperty)

  //infix patters (only works when you have two thins in pattern)

  case class Or[A,B](a:A ,b:B)
  val either =  Or(1,"one")
  val desc= either match  {
    case number Or string => s"$number is written as $string"
  }
  println(desc)

  //decomposing sequences
  val vararg= numbers match {
    case List(1,_*) => "starting with 1"
  }
  println(vararg)

  abstract class MyList[+A]{
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail:MyList[A]) extends MyList[A]

  object MyList{
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] = {
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
    }
  }

  val myList=Cons(1,Cons(2,Cons(3,Empty)))

  val decomposed = myList match {
    case MyList(1,2,_*)=>"This is starting with 1 and 2"
    case _ => "this is something else"
  }

  println(decomposed)

  //custom return types for unapply
  abstract class Wrapper[T] {
    def isEmpty:Boolean
    def get:T
  }

  object PersonWrapper {
    def unapply(arg: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false

      override def get: String = arg.name
    }
  }

  println(bob match {
    case PersonWrapper(name)=>s"This is $name"
    case _=> "nothing "
  })

}
