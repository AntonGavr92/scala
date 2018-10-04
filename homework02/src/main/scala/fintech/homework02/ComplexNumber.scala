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
