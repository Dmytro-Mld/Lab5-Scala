import CommutativeRing.CommutativeRingOps
import ComplexNumbers.ComplexNumber
import scala.language.implicitConversions

object ComplexNumbers {
  case class ComplexNumber(real: Long, imagine: Long)
}

trait CommutativeRing[A] {
  def sum(a: A, b:A): A
  def mult(a: A, b: A): A
  def inverse(a: A): A
  def zero: A
  def one: A
}

object CommutativeRing {
  implicit val ComplexNumbersFormCommutativeRing: CommutativeRing[ComplexNumber] = new CommutativeRing[ComplexNumber] {
    override def sum(a: ComplexNumber, b: ComplexNumber): ComplexNumber =
      ComplexNumber(a.real + b.real, a.imagine + b.imagine)

    override def mult(a: ComplexNumber, b: ComplexNumber): ComplexNumber =
      ComplexNumber(a.real * b.real, a.imagine * b.imagine)

    override def inverse(a: ComplexNumber): ComplexNumber =
      ComplexNumber(-a.real, -a.imagine)

    override def zero: ComplexNumber =
      ComplexNumber(0, 0)

    override def one: ComplexNumber =
      ComplexNumber(1, 1)
  }

  class CommutativeRingOps[A](a: A)(implicit g: CommutativeRing[A]) {

    def ++(b: A): A =
      g.sum(a, b)


    def **(b: A): A = {
      g.mult(a, b)
    }

    def inv: A = {
      g.inverse(a)
    }

    def zero: A = {
      g.zero
    }

    def one: A = {
      g.one
    }
  }
}

object CommutativeRingOps {
  implicit def commutativeRingSyntax[A](a: A)(implicit g: CommutativeRing[A]): CommutativeRingOps[A] = {
    new CommutativeRingOps[A](a)
  }
}