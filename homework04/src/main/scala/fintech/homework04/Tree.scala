package fintech.homework04

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold

  val MaxCountLeafInBranch = 2

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    @tailrec
    def foldTree(folded: Option[B], treeSeq: Queue[Tree[A]])(f: A => B)(g: (B, B) => B): B = {
      if (treeSeq.isEmpty) {
        folded.get
      } else {
        val (tree, newQueue) = treeSeq.dequeue
        tree match {
          case leaf: Leaf[A] => foldTree(fold(leaf.value, folded), newQueue)(f)(g)
          case branch: Branch[A] => foldTree(folded, newQueue ++ List(branch.right, branch.left))(f)(g)
        }
      }
    }

    def fold(a: A, b: Option[B]): Option[B] = {
      b.map(value => g(value, f(a))).orElse(Option(f(a)))
    }

    foldTree(Option.empty, Queue(t))(f)(g)
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((x, y) => x + y)
  }

  def max(t: Tree[Int]): Int = {
    fold(t)(identity)((x, y) => math.max(x, y))
  }

  def depth[A](t: Tree[A]): Int = {
    size(t) / MaxCountLeafInBranch + 1
  }

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case leaf: Leaf[A] => new Leaf[B](f(leaf.value))
      case branch: Branch[A] => new Branch[B](map(branch.left)(f), map(branch.right)(f))
    }
  }

  def equals[B](left: Tree[B], right: Tree[B]): Boolean = {

    val DefaultState = true

    @tailrec
    def equalsTree(curState: Boolean, queueTuple: Queue[(Tree[B], Tree[B])]): Boolean = {
      if (!curState || queueTuple.isEmpty)
        curState
      else {
        val ((left, right), newQueue) = queueTuple.dequeue
        (left, right) match {
          case (left: Leaf[B], right: Leaf[B]) => left == right
          case (leftB: Branch[B], rightB: Branch[B]) => equalsTree(curState, newQueue ++ List((leftB.left, rightB.left), (leftB.right, rightB.right)))
          case _ => false
        }
      }
    }

    equalsTree(DefaultState, Queue((left, right)))
  }
}
