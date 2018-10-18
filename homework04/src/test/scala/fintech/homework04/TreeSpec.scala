package fintech.homework04

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  val tree = new Branch[Int](
    new Branch[Int](new Leaf[Int](3), new Leaf[Int](4)),
    new Leaf[Int](1)
  )

  val doubleTree = new Branch[Double](
    new Branch[Double](new Leaf[Double](3.0), new Leaf[Double](4.0)),
    new Leaf[Double](1.0)
  )

  "Tree" should "be fold correct" in {
    Tree.fold(tree)(x => x.toDouble)((x, y) => x + y) shouldBe 8.0
  }

  "Method size of Tree" should "be work correct" in {
    Tree.size(tree) shouldBe 3
  }

  "Method max of Tree" should "be get maximal value of leaf in tree" in {
    Tree.max(tree) shouldBe 4
  }

  "Method depth of Tree" should "be work correct" in {
    Tree.depth(tree) shouldBe 2
  }

  "Method map of Tree" should "be work correct" in {
    Tree.equals(Tree.map(tree)(x => x.toDouble), doubleTree) shouldBe true
    Tree.size(Tree.map(tree)(x => x.toDouble)) shouldBe Tree.size(tree)
    Tree.size(Tree.map(tree)(x => x.toDouble)) shouldBe Tree.size(tree)
    Tree.depth(Tree.map(tree)(x => x.toDouble)) shouldBe Tree.depth(tree)
  }
}
