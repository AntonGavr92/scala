package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

object Eq {

  implicit val intEq: Eq[Int] = (lft: Int, rgt: Int) => lft == rgt
  implicit val strEq: Eq[String] = (lft: String, rgt: String) => lft == rgt
  implicit val doubleEq: Eq[Double] = (lft: Double, rgt: Double) => lft == rgt
  implicit val bigDecimalEq: Eq[BigDecimal] = (lft: BigDecimal, rgt: BigDecimal) => lft == rgt

  implicit def iteratorEq[A](implicit eq: Eq[A]): Eq[Iterator[A]] = (lft: Iterator[A], rgt: Iterator[A]) => {
    lft.forall(item => eq.equiv(item, rgt.next()))
  }

  implicit class EqList[A](leftList: List[A]) {
    def ====(rightList: List[A])(implicit eqItems: Eq[Iterator[A]]): Boolean = {
      leftList.size == rightList.size && eqItems.equiv(leftList.iterator, rightList.iterator)
    }
  }

  implicit class EqSet[A](leftSet: Set[A]) {
    def ====(rightSet: Set[A])(implicit eqItems: Eq[Iterator[A]]): Boolean = {
      leftSet.size == rightSet.size && eqItems.equiv(leftSet.iterator, rightSet.iterator)
    }
  }

  implicit class EqMap[A, B](leftMap: Map[A, B]) {
    def ====(rightMap: Map[A, B])(implicit eqKeys: Eq[Iterator[A]], eqValues: Eq[Iterator[B]]): Boolean = {
      leftMap.size == rightMap.size &&
        eqKeys.equiv(leftMap.keys.iterator, rightMap.keys.iterator) &&
        eqValues.equiv(leftMap.values.iterator, rightMap.values.iterator)
    }
  }

  implicit class EqComplexNumber(leftComplexNumber: ComplexNumber) {
    def ====(rightComplexNumber: ComplexNumber, scale: Int = 2)(implicit eq: Eq[BigDecimal]): Boolean = {
      eq.equiv(
        BigDecimal(leftComplexNumber.real).setScale(scale, BigDecimal.RoundingMode.HALF_UP),
        BigDecimal(rightComplexNumber.real).setScale(scale, BigDecimal.RoundingMode.HALF_UP)
      ) &&
        eq.equiv(
          BigDecimal(leftComplexNumber.imaginary).setScale(scale, BigDecimal.RoundingMode.HALF_UP),
          BigDecimal(rightComplexNumber.imaginary).setScale(scale, BigDecimal.RoundingMode.HALF_UP)
        )
    }
  }

}

class ComplexNumber (val real: Double, val imaginary: Double){

  def + (another: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real + another.real, imaginary + another.imaginary)
  }

  def - (another: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real - another.real, imaginary - another.imaginary)
  }

  def * (another: ComplexNumber): ComplexNumber = {
    new ComplexNumber(
      real * another.real - imaginary * another.imaginary,
      real * another.real + imaginary * another.imaginary
    )
  }

  def ~ (value: Int): ComplexNumber = {
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