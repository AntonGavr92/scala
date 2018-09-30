package fintech.homework02

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
    Stream.iterate(this)(number => number * this).apply(value - 1)
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
