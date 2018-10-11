package fintech.homework03
import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  it should "work well with strings" in {
    val tree: PrefixTree[Char, Int] = new PrefixTreeImpl(Option.empty, Map())

    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)
    with42.sub("ab").sub("cd").get should be (42)

    val withDouble: PrefixTree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get should be (42)
    withDouble.sub("ab").sub("cde").get should be (13.0)
  }

  it should "work well with int" in {
    val tree: PrefixTree[Int, Int] = new PrefixTreeImpl(Option.empty, Map())
    val with42: PrefixTree[Int, Int] = tree.put(Seq(1, 2, 3, 4), 42)
    with42.sub(Seq(1,2)).sub(Seq(3,4)).get should be (42)
  }

  "Method get" should "be work correct" in {
    val tree: PrefixTree[Char, Int] = new PrefixTreeImpl(Option.empty, Map())
    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)
    intercept[NoSuchElementException] {
      with42.sub("ab").get
    }
    Option(with42.sub("ab").sub("cd").get) should be (Option(42))
  }


}
