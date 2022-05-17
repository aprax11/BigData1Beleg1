package assignment1

import list.implementation.SinglyLinkedIntList
import org.scalatest.funsuite.AnyFunSuite

class ProblemsApplyHOFTest extends AnyFunSuite {

  test("testSumOddNumbers") {
    assert(ProblemsApplyHOF.sumOddNumbers(SinglyLinkedIntList(1, 2, 3, 4, 5)) === 9)
  }

  test("testCountEvenNumbers") {
    assert(ProblemsApplyHOF.countEvenNumbers(SinglyLinkedIntList(1, 2, 3, 4, 5)) === 2)
  }

  test("testMultiplyAndFilterEven") {
    assert(ProblemsApplyHOF.multiplyAndFilterEven(SinglyLinkedIntList(1, 2, 3), 3) === SinglyLinkedIntList(6))
  }

  test("testFindMin") {
    assert(ProblemsApplyHOF.findMin(SinglyLinkedIntList(3, 5, 1, 2, 3, 4, 5)) === 1)
  }
}
