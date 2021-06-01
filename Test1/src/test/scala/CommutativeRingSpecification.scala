import CommutativeRingOps._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import ComplexNumbers.ComplexNumber

object CommutativeRingSpecification extends Properties("CommutativeRing") {

  implicit val arb: Arbitrary[ComplexNumber] = Arbitrary {
    for {
      re <- Gen.long
      im <- Gen.long
    } yield {
      ComplexNumber(re, im)
    }
  }

  val f: CommutativeRing[ComplexNumber] = CommutativeRing.ComplexNumbersFormCommutativeRing

  property("invSum") = forAll { i: ComplexNumber =>
    f.sum(i.inv, i) == i.zero
  }

  property("assocForSum") = forAll { (a: ComplexNumber, b: ComplexNumber, c: ComplexNumber) =>
    f.sum(f.sum(a, b), c) == f.sum(a, f.sum(b, c))
  }

  property("commutative") = forAll { (i: ComplexNumber, j: ComplexNumber) =>
    f.sum(i, j) ==  f.sum(j, i)
  }

  property("zero") = forAll { i: ComplexNumber =>
    f.sum(i.zero, i) == i
  }

  property("assocForMult") = forAll { (a: ComplexNumber, b: ComplexNumber, c: ComplexNumber) =>
    f.mult(f.mult(a, b), c) == f.mult(a, f.mult(b, c))
  }

  property("one") = forAll { i: ComplexNumber =>
    f.mult(i.one, i) == i
  }
}