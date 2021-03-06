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
    "return a random int paired with a value between 0 and 1" in {
      prop { (seed: Int) =>
        val ((d, i), _) = RNG.doubleInt(RNG.Simple(seed))
        d must beBetween(0.0, 1.0).excludingEnd
      }
    }
  }

  "intDouble" should {
    "return a random int paired with a value between 0 and 1" in {
      prop { (seed: Int) =>
        val ((i, d), _) = RNG.intDouble(RNG.Simple(seed))
        d must beBetween(0.0, 1.0).excludingEnd
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
        if (count > 2)
          values.distinct must have size(be_>(1))
        values must have size count
      }.setGen2(Gen.posNum[Int])
    }
  }

  "nonNegativeLessThan" should {
    "return values less than n" in {
      prop { (seed: Int) =>
        val (value, _) = RNG.nonNegativeLessThan(6)(RNG.Simple(seed))
        value must beBetween(0, 5)
      }
    }
  }

  "simulateMachine" should {
    "operate the machine based on the inputs" in {
      val container = State.simulateMachine(List(
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn
      ))
      val ((candies, coins), _) = container.run(Machine(true, 5, 10))
      candies === 1
      coins === 14
    }
    "support empty inputs" in {
      val container = State.simulateMachine(Nil)
      val ((candies, coins), _) = container.run(Machine(true, 5, 10))
      candies === 5
      coins === 10
    }
    "ignore wrong order of input" in {
      val container = State.simulateMachine(List(
        Turn, Coin
      ))
      val ((candies, coins), _) = container.run(Machine(true, 5, 10))
      candies === 5
      coins === 11
    }
    "ignore inputs when it's out of candies" in {
      val tenCoinTurns = List.fill(10)(List(Coin, Turn)).flatten
      val container = State.simulateMachine(tenCoinTurns)
      val ((candies, coins), _) = container.run(Machine(true, 5, 10))
      candies === 0
      coins === 15
    }
  }
}
