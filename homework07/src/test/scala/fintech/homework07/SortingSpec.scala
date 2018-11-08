package fintech.homework07
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class SortingSpec extends FlatSpec with Matchers {
  val mutableList = mutable.MutableList(2, 1, 3, 6, 4, 5)
  val sortedMutableList = mutable.MutableList(1, 2, 3, 4, 5, 6)
  val intComparator = (a: Int,b: Int) => if(a < b) 1 else -1


  "Merge sorting of Sorting object" should "be work correct" in {
    Sorting.mergeSort(mutableList, intComparator) shouldBe sortedMutableList
  }

  "Quick sorting of Sorting object" should "be work correct" in {
    Sorting.quickSort(mutableList, intComparator) shouldBe sortedMutableList
  }
}
