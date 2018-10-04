package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {

  val first = new ComplexNumber(3, 3)
  val second = new ComplexNumber(2, 2)

  "Complex numbers" should "be equals if they real and imaginary part is equals" in {
    first shouldBe new ComplexNumber(3, 3)
  }

  "Sum of complex numbers" should "be correct" in {
    (first + second) shouldBe new ComplexNumber(5, 5)
  }

  "Difference of complex numbers" should "be correct" in {
    (first - second) shouldBe new ComplexNumber(1, 1)
  }

  "Multiple of complex numbers" should "be correct" in {
    (first * second) shouldBe new ComplexNumber(0, 12)
  }

  "ToString method of complex number" should "be correct" in {
    first toString() shouldBe "3.0 + 3.0i"
  }

  "Positive pow of complex number" should "be correct" in {
    (second ~ 3 real) should be < -16.0
    (second ~ 3 real) should be >= -16.1
    (second ~ 3 imaginary) should be <= 16.1
    (second ~ 3 imaginary) should be > 16.0
  }

  "Negative pow of complex number" should "be correct" in {
    (second ~ -3 real) should be <= -0.031
    (second ~ -3 real) should be > -0.032
    (second ~ -3 imaginary) should be <= -0.031
    (second ~ -3 imaginary) should be > -0.032
  }

  "Pow 0 of complex number" should "be correct" in {
    (second ~ 0 real) shouldBe 1
    (second ~ 0 imaginary) shouldBe 0
  }
}
