package fintech.homework04
import fintech.homework06.{ComplexNumber, Scale}
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.Eq._

class EqSpec extends FlatSpec with Matchers {

  "Eq with list of ints" should "be work correct" in {
    List(1) ==== List(2) shouldBe false
    List(1, 2) ==== List(1, 2) shouldBe true
  }

  "Eq with set of ints" should "be work correct" in {
    Set(1, 2) ==== Set(2, 1) shouldBe false
    Set(1, 2) ==== Set(1, 2) shouldBe true
  }

  "Eq with map of ints" should "be work correct" in {
    Map(1 -> 1) ==== Map(2 -> 1) shouldBe false
    Map(1 -> 1) ==== Map(1 -> 1) shouldBe true
  }

  "Eq with complex numbers" should "be work correct" in {
    val first = new ComplexNumber(3, 3)
    val second = new ComplexNumber(2, 2)
    implicit val scale: Scale = Scale(4)
    (first ==== second) shouldBe false
    (first - new ComplexNumber(1, 1)) ~ 3  ==== (second ~ 3) shouldBe true
  }

}
