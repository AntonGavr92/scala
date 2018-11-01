package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

case class Scale(value: Int)

object Eq {

  implicit val intEq: Eq[Int] = (lft: Int, rgt: Int) => lft == rgt
  implicit val strEq: Eq[String] = (lft: String, rgt: String) => lft == rgt

  implicit def doubleEq(implicit scale: Scale = Scale(2)): Eq[Double] = (lft: Double, rgt: Double) =>
    BigDecimal(lft).setScale(scale.value, BigDecimal.RoundingMode.HALF_UP) ==
      BigDecimal(rgt).setScale(scale.value, BigDecimal.RoundingMode.HALF_UP)

  implicit def listEq[A](implicit eq: Eq[A]): Eq[List[A]] = {
    (lft: List[A], rgt: List[A]) => lft.zip(rgt).forall(pair => eq.equiv(pair._1, pair._2))
  }

  implicit def mapEq[A, B](implicit eqA: Eq[A], eqB: Eq[B]): Eq[Map[A, B]] = {
    (lft: Map[A, B], rgt: Map[A, B]) =>
      lft.values.zip(rgt.values).forall(pair => eqB.equiv(pair._1, pair._2)) &&
        lft.keys.zip(rgt.keys).forall(pair => eqA.equiv(pair._1, pair._2))
  }

  implicit def setEq[A](implicit eq: Eq[A]): Eq[Set[A]] = {
    (lft: Set[A], rgt: Set[A]) => lft.zip(rgt).forall(pair => eq.equiv(pair._1, pair._2))
  }

  implicit def complexEq(implicit eq: Eq[Double]): Eq[ComplexNumber] = {
    (lft: ComplexNumber, rgt: ComplexNumber) => eq.equiv(lft.imaginary, rgt.imaginary) && eq.equiv(lft.real, rgt.real)
  }

  implicit class EqImpl[A](leftEq: A) {
    def ====(rightEq: A)(implicit eq: Eq[A]): Boolean = {
      eq.equiv(leftEq, rightEq)
    }
  }

}

class ComplexNumber(val real: Double, val imaginary: Double) {

  def +(another: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real + another.real, imaginary + another.imaginary)
  }

  def -(another: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real - another.real, imaginary - another.imaginary)
  }

  def *(another: ComplexNumber): ComplexNumber = {
    new ComplexNumber(
      real * another.real - imaginary * another.imaginary,
      real * another.real + imaginary * another.imaginary
    )
  }

  def ~(value: Int): ComplexNumber = {
    val abs = Math.sqrt(real * real + imaginary * imaginary)
    val arg = if (real > 0)
      Math.atan2(imaginary, real)
    else if (imaginary > 0)
      Math.atan2(imaginary, real) + Math.PI
    else
      Math.atan2(imaginary, real) - Math.PI
    new ComplexNumber(
      Math.pow(abs, value) * Math.cos(value * arg),
      Math.pow(abs, value) * Math.sin(value * arg)
    )
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[ComplexNumber]

  override def equals(other: Any): Boolean = other match {
    case that: ComplexNumber =>
      (that canEqual this) &&
        real == that.real &&
        imaginary == that.imaginary
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(real, imaginary)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = {
    real + " + " + imaginary + "i"
  }
}