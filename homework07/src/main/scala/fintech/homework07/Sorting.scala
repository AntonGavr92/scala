package fintech.homework07

import scala.collection.mutable

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  * использую *подходящие* *мутабельные* коллекции
  */

object Sorting {

  def mergeSort[A](list: mutable.ArrayBuffer[A])(implicit comparator: Ordering[A]): mutable.ArrayBuffer[A] = {

    case class MergeSortState(item: Option[A], leftIndex: Int, rightIndex: Int)

    def merge(leftList: mutable.ArrayBuffer[A], rightList: mutable.ArrayBuffer[A]): mutable.ArrayBuffer[A] = {

      def compare(left: Option[A], right: Option[A], currentState: MergeSortState = MergeSortState(Option.empty, 0, 0)): MergeSortState = {
        if (left.isEmpty)
          MergeSortState(right, currentState.leftIndex, currentState.rightIndex + 1)
        else if (right.isEmpty)
          MergeSortState(left, currentState.leftIndex + 1, currentState.rightIndex)
        else if (comparator.compare(left.get, right.get) < 0)
          MergeSortState(left, currentState.leftIndex + 1, currentState.rightIndex)
        else
          MergeSortState(right, currentState.leftIndex, currentState.rightIndex + 1)
      }

      def emptyOrValue(l: mutable.ArrayBuffer[A], index: Int): Option[A] = {
        if (l.size > index)
          Option(l(index))
        else
          Option.empty
      }

      Stream.iterate(compare(Option(leftList.head), Option(rightList.head)))(res =>
        compare(emptyOrValue(leftList, res.leftIndex), emptyOrValue(rightList, res.rightIndex), res))
        .takeWhile(res => res.leftIndex + res.rightIndex < leftList.size + rightList.size + 1)
        .map(_.item.get)
        .to
    }

    val middle = list.length / 2
    if (middle == 0) list
    else {
      val (left, right) = list.splitAt(middle)
      val sorted = merge(mergeSort(left), mergeSort(right))
      for (i <- sorted.indices) list(i) = sorted(i)
      list
    }
  }

  def quickSort[A](list: mutable.ArrayBuffer[A])(implicit comparator: Ordering[A]): mutable.ArrayBuffer[A] = {

    def sort(start: Int, finish: Int): mutable.ArrayBuffer[A] = {
      if (start >= finish) return list
      val pivot = partition(start, finish)
      sort(start, pivot - 1)
      sort(pivot + 1, finish)
    }

    def partition(start: Int, finish: Int): Int = {
      val pivot = list(finish)
      var i = start - 1
      var j = start

      while (j < finish) {
        if (comparator.compare(list(j), pivot) < 0) {
          i += 1

          val temp = list(i)
          list(i) = list(j)
          list(j) = temp
        }
        j += 1
      }
      val swapTemp = list(i + 1)
      list(i + 1) = list(finish)
      list(finish) = swapTemp
      i + 1
    }

    sort(0, list.size - 1)

  }

}
