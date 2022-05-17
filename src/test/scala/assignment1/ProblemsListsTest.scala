package assignment1

import list.implementation._
import org.scalatest.funsuite.AnyFunSuite

class ProblemsListsTest extends AnyFunSuite{

  test("duplicate Num") {
    assert(ProblemsLists.duplicateNum(5, 3) === SinglyLinkedIntList(5, 5, 5))
  }

  test("duplicate Equal Numbers") {
    assert(ProblemsLists.duplicateEqualNumbers(3, SinglyLinkedIntList(5, 4, 7, 2)) ===
      SinglyLinkedIntList(5, 4, 4, 4, 7, 2, 2, 2))
  }

  test("merge Lists") {
    assert(ProblemsLists.merge(SinglyLinkedIntList(1,4,6,9), SinglyLinkedIntList(4,7,8,9)) ===
      SinglyLinkedIntList(1, 4, 4, 6, 7, 8, 9, 9))
  }

  test("merge Lists Empty 1") {
    assert(ProblemsLists.merge(Empty, SinglyLinkedIntList(4,7,8,9)) ===
      SinglyLinkedIntList(4,7,8,9))
  }

  test("merge Lists Empty 2") {
    assert(ProblemsLists.merge(SinglyLinkedIntList(4,7,8,9), Empty) ===
      SinglyLinkedIntList(4,7,8,9))
  }

  test("split Lists") {
    val (l1,l2)= ProblemsLists.splitList(SinglyLinkedIntList(14,327,18,14,9,110,12,345,123,124))
    val (s1,s2)= (l1.insertionSort,l2.insertionSort)
    assert(s1 == SinglyLinkedIntList(9,14,14,18,327),s2==SinglyLinkedIntList(12,110,123,124,345))
  }

  test("split Lists uneven number of Elements") {
    val (l1,l2)= ProblemsLists.splitList(SinglyLinkedIntList(14,327,18,14,9,110,12,345,123,124,145))
    val (s1,s2)= (l1.insertionSort,l2.insertionSort)
    assert(s1 == SinglyLinkedIntList(9,14,14,18,110,327),s2==SinglyLinkedIntList(12,123,124,145,345))
  }
  test("split Lists Empty") {
    assert(ProblemsLists.splitList(SinglyLinkedIntList())==(SinglyLinkedIntList(),SinglyLinkedIntList()))
  }

  test("mergeSort") {
    assert(ProblemsLists.mergeSort(SinglyLinkedIntList(14,327,18,14,9,110,12,345,123,124,276)) ===
      SinglyLinkedIntList(9,12,14,14,18,110,123,124,276,327,345))
  }

  test("minBagsCount 1") {

    val t= Cons(4, Cons(8, Cons(1, Cons(4, Cons(2, Cons(1, Empty))))))
    val tr= ProblemsLists.minBagsCount(10,t)
    assert(tr === 2)
  }

  test("minBagsCount 2") {

    val t= Cons(9, Cons(8, Cons(2, Cons(2, Cons(5, Cons(4, Empty))))))
    val tr= ProblemsLists.minBagsCount(10,t)
    assert(tr === 4)
  }

  test("minBagsCount 3") {

    val t= Cons(2, Cons(5, Cons(4, Cons(7, Cons(1, Cons(3, Cons(8, Empty)))))))
    val tr= ProblemsLists.minBagsCount(10,t)
    assert(tr === 3)
  }

  test("minBagsCount 4") {

    val w= Cons(4,Cons(4,Cons(3,Cons(2,Cons(2,Cons(1,Cons(1,Cons(1,Empty))))))))
    val x= ProblemsLists.minBagsCount(10,w)
    assert(x === 2)
  }
  test("packProblem 1") {

    val w= Cons(4,Cons(4,Cons(3,Cons(2,Cons(2,Cons(1,Cons(1,Cons(1,Empty))))))))
    val x= ProblemsLists.packProblem(10,w)
    assert(x === 10)
  }

  test("packProblem 2") {

    val w= Cons(3, Cons(3, Cons(4,Cons(4,Cons(8,Empty)))))
    val x= ProblemsLists.packProblem(9,w)
    assert(x === 8)
  }

  test("packProblem 3") {

    val w= Cons(4,Cons(4,Cons(3,Cons(2,Cons(2,Cons(1,Cons(1,Cons(1,Empty))))))))
    val x= ProblemsLists.packProblem(20,w)
    assert(x === 18)
  }

  test("packProblem 4") {

    val w= Cons(4,Cons(4,Cons(3,Cons(6,Cons(5,Empty)))))
    val x= ProblemsLists.packProblem(2,w)
    assert(x === 0)
  }
}
