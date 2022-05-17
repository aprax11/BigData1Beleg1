package assignment1

import list.traits.IntList

/**
  * Ich habe mit Lara Neubauer da Costa Schertel zusammen gearbeitet
  */

/**
  * Complete the following exercises to practice the usage of higher order functions.
  */
object ProblemsApplyHOF {

  /**
    * multiplyAndFilterEven should multiply all elements of the IntList by
    * the factor x and filter all element that are even
    */
  def multiplyAndFilterEven(l: IntList, x: Int): IntList = {
    l.map(a => a * x).filter(b => b % 2 == 0)
  }

  /**
    * findMin should find the smallest element of a list
    */
  def findMin(l: IntList): Int = {
    l.insertionSort.head
  }

  /**
    * sumOddNumbers should sum up all odd numbers of a list
    */
  def sumOddNumbers(l: IntList): Int = {
    l.filter(a => a % 2 == 1).reduceRight((b, c) => b + c)
  }

  /**
    * countEvenNumbers should count all even numbers of a list
    */
  def countEvenNumbers(l: IntList): Int = {
    l.filter(a => a % 2 == 0).size
  }
}
