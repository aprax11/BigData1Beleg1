package list.implementation

import list.traits.IntList

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other match{
    
    case Empty => this
    case Cons(h,t) => Cons(h,prefix(t)) 
  }
  override def size: Int = this match{
    
    case Empty => 0
    case _ => 1+tail.size
  }

  /** ------------------------------------------
    *
    * Assignment 1
    *
    * ------------------------------------------ */


  override def map(mapFunc: Int => Int): IntList = this match {
    case Cons(head, tail) => Cons(mapFunc(head), tail.map(mapFunc))
    case Empty => Empty
  }

  override def filter(filterFunc: Int => Boolean): IntList =  this match {
    case Cons(head, tail) => if(filterFunc(head)) Cons(head, tail.filter(filterFunc)) else tail.filter(filterFunc)
    case Empty => Empty
  }

  
  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => initial
    case Cons(head, tail) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
  }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = this match {
    case Cons(head, Empty) => head
    case Cons(head1, Cons(head2, tail)) => Cons(reduceFunc(head1, head2), tail).reduceLeft(reduceFunc)
  }

  override def forAll(predicateFunc: Int => Boolean): Boolean = this match {
    case Cons(head, Empty) => predicateFunc(head)
    case Cons(head, tail) => if(predicateFunc(head)) tail.forAll(predicateFunc)
    else false
  }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => initial
    case Cons(head, tail) => reduceFunc(head, tail.foldRight(initial)(reduceFunc))
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => -1
    case Cons(head, Empty) => head
    case Cons(head, tail) => reduceFunc(head, tail.reduceRight(reduceFunc))
  }

  override def insertionSort: IntList = {
    def compare(h1:Int, list:IntList):IntList = list match {
      case Empty => Cons(h1, Empty)
      case Cons(h2, tail) => if(h1 <= h2) Cons(h1, list) else Cons(h2, compare(h1, tail))
    }
    def apply (list: IntList): IntList = list match {
      case Empty => Empty
      case Cons(h,t) => compare(h, apply(t))
    }
    apply(this)
  }



  override def insertSorted(elem: Int): IntList = this match {
    case Empty => Cons(elem, Empty)
    case Cons(h, Empty) => if(h < elem) Cons(h, Cons(elem, Empty)) else Cons(elem,Cons(h, Empty))
    case Cons(h, Cons(h2, t)) => if(elem > h && h2 > elem) Cons(h, Cons(elem, Cons(h2, t))) else if(elem < h) Cons(elem, Cons(h, Cons(h2,t)))
    else Cons(h, Cons(h2, t.insertSorted(elem)))
  }

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = this match {
    case Empty => initial
    case Cons(head, tail) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
  }
}