package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  /*
    *
    *
    */
  def apply(elem:A): Boolean =contains(elem)
  def contains(elem:A):Boolean
  def +(elem:A):MySet[A]
  def ++(anotherSet:MySet[A]):MySet[A] //union

  def map[B](f:A=>B):MySet[B]
  def flaMap[B](f:A=>MySet[B]):MySet[B ]
  def filter(predicate:A=>Boolean):MySet[A]
  def foreach(f:A=>Unit):Unit

  def -(elem : A):MySet[A]
  def --(anotherSet:MySet[A]):MySet[A] //difference
  def &(anotherSet:MySet[A]):MySet[A]

  def unary_! : MySet[A]

}


  class EmptySet[A] extends MySet[A] {
    override def contains(elem: A): Boolean = false
    override def +(elem: A): MySet[A] = new NonEmptySet[A](elem,this)
    override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

    override def map[B](f: A => B): MySet[B] = new EmptySet[B]
    override def flaMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
    override def filter(predicate: A => Boolean): MySet[A] = this
    override def foreach(f: A => Unit): Unit = ()

    override def -(elem: A): MySet[A] = this

    override def --(anotherSet: MySet[A]): MySet[A] = this

    override def &(anotherSet: MySet[A]): MySet[A] = this

    override def unary_! : MySet[A] = new PropertyBasedSet[A](_ =>true)

}

//All elements of type A which satisfy a property
// {x in A | property }
class PropertyBasedSet[A](property:A => Boolean) extends MySet[A]{
  override def contains(elem: A): Boolean = property(elem)

  //{x in A | property(x) + element = {x in A | property(x) || x == element }}
  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem )

  //{x in A | property(x) ++ set} => { x in A | property(x) || set contains x }
  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x=>property(x) || anotherSet(x))

  //all integers  => (_ % 3) = [0,1,2]
  override def map[B](f: A => B): MySet[B] = politelyFail
  override def flaMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def foreach(f: A => Unit): Unit = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x=>property(x) && predicate(x))

  override def -(elem: A): MySet[A] = filter(x=>x !=elem)
  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x=> !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")

}


class NonEmptySet[A](head:A,tail:MySet[A]) extends MySet[A]{

  override def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if(this contains elem)
      this
    else
      new NonEmptySet[A](elem,this)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = (tail map f)+f(head)

  override def flaMap[B](f: A => MySet[B]): MySet[B] = (tail flaMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }


  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(elem: A): MySet[A] =
    if(head==elem)
      tail
    else
      tail - elem + head


  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) //intersecting and filter is same thing
  override def unary_! : MySet[A] = new PropertyBasedSet[A](x=> !this.contains(x))
}

object MySet {
  /*
  val s= MySet(1,2,3) = buildSet(seq(1,2,3),[])
  = buildSet(seq(2,3),[] + 1)
  = buildSet(seq(3),[1] + 2)
  = buildSet(seq(),[1,2] + 3)
  = [1,2,3]
   */
  def apply[A](values:A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq:Seq[A],acc:MySet[A]):MySet[A]= {
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail,acc + valSeq.head)
    }
    buildSet(values.toSeq,new EmptySet[A])
  }
}


object MySetPlayground extends App{
  val s=MySet(1,2,3,4)
  //s + 5 ++ MySet(-1,-2) + 3 map(x => x) flaMap (x => MySet(x,x*10)) filter (x=>x%2 ==0) foreach println


  val negative = !s //
  //println(negative.foreach(println))
  println(negative(2))
  println(negative(5))
}