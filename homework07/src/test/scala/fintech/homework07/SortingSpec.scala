package fintech.homework07
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.util.Random

class SortingSpec extends FlatSpec with Matchers {
  val arrayBuffer = mutable.ArrayBuffer(2, 1, 3, 6, 4, 5)
  val sortedArrayBuffer = mutable.ArrayBuffer(1, 2, 3, 4, 5, 6)

  "Merge sorting of Sorting object" should "be work correct" in {
    Sorting.mergeSort(arrayBuffer) shouldBe sortedArrayBuffer
  }

  it should "mergeSort correctly" in {
    def isSorted(arr: Seq[Double]): Boolean = 1.until(arr.length).forall(i => arr(i - 1) <= arr(i))
    val randomSeq = mutable.ArrayBuffer.fill(100)(Random.nextDouble())
    Sorting.mergeSort(randomSeq)
    assert(isSorted(randomSeq))
  }

  "Quick sorting of Sorting object" should "be work correct" in {
    Sorting.quickSort(arrayBuffer) shouldBe sortedArrayBuffer
  }

  it should "quickSort correctly" in {
    def isSorted(arr: Seq[Double]): Boolean = 1.until(arr.length).forall(i => arr(i - 1) <= arr(i))
    val randomSeq = mutable.ArrayBuffer.fill(100)(Random.nextDouble())
    Sorting.quickSort(randomSeq)
    assert(isSorted(randomSeq))
  }

}
