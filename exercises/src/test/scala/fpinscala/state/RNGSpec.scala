package fpinscala.state

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck._

class RNGSpec extends Specification with ScalaCheck {

  "nonNegativeInt" should {
    "only return positive numbers or zero" in {
      prop { (seed: Int) => RNG.nonNegativeInt(RNG.Simple(seed))._1 >= 0 }
    }
  }

  "double" should {
    "return values between 0 and 1" in {
      prop { (seed: Int) =>
        val (d, _) = RNG.double(RNG.Simple(seed))
        d must beBetween(0.0, 1.0).excludingEnd
      }
    }
  }

  "intDouble" should {
    "return a non-negative values paired with a value between 0 and 1" in {
      prop { (seed: Int) =>
        val ((d, i), _) = RNG.doubleInt(RNG.Simple(seed))
        i must be beGreaterThanOrEqualTo 0
        d must beBetween(0.0, 1.0).excludingEnd
      }
    }
  }

  "intDouble" should {
    "return a non-negative values paired with a value between 0 and 1" in {
      prop { (seed: Int) =>
        val ((i, d), _) = RNG.intDouble(RNG.Simple(seed))
        d must beBetween(0.0, 1.0).excludingEnd
        i must be beGreaterThanOrEqualTo 0
      }
    }
  }

  "double3" should {
    "return 3 doubles" in {
      prop { (seed: Int) =>
        val ((d1, d2, d3), _) = RNG.double3(RNG.Simple(seed))
        d1 must beBetween(0.0, 1.0).excludingEnd
        d2 must beBetween(0.0, 1.0).excludingEnd
        d3 must beBetween(0.0, 1.0).excludingEnd
      }
    }
  }

  "ints" should {
    "return random values" in {
      prop { (seed: Int, count: Int) =>
        val (values, _) = RNG.ints(count)(RNG.Simple(seed))
        values must contain(be_>=(0)).forall
      }.setGen2(Gen.posNum[Int])
    }
  }
}
