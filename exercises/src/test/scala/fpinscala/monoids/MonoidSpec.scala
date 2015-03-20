package fpinscala.monoids

import java.util.concurrent.Executors

import org.specs2.mutable._
import org.specs2.specification.AfterAll

import fpinscala.parallelism.Nonblocking._
import fpinscala.testing._
import Monoid._

class MonoidSpec extends Specification with AfterAll {

  def verifyLaws[A](name: String, m: Monoid[A])(gen: => Gen[A]) = {
    name should {
      "verify monoid laws" in {
        monoidLaws(m, gen).quickRun match {
          case Falsified(label, _, _) =>
            failure(s"Failed to verify the law '${label.getOrElse("unnamed law")}'")
          case _ => success
        }
      }
    }
  }

  verifyLaws("string concat", stringMonoid) {
    val text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit"
    Gen.domain(text.split(",? +"))
  }

  verifyLaws("int addition", intAddition)(Gen.int)
  verifyLaws("int multiplication", intMultiplication)(Gen.int)
  verifyLaws("boolean or", booleanOr)(Gen.boolean)
  verifyLaws("boolean and", booleanAnd)(Gen.boolean)

  verifyLaws("option monoid", optionMonoid[Int]) {
    Gen.int.map(Some(_)) -> 0.8 union Gen.unit(None) -> 0.2
  }

  // The zero law cannot be simply verified,
  // because while the result of going through
  // the f and identity will be the same as only
  // going through f ( f(identity(x)) == f(x) ),
  // the functions themselves are not equal.
  // verifyLaws("endoMonoid", endoMonoid[Int]) {
  //   Gen.values(_ * 2, _ + 100, _ / 2, _ % 100)
  // }

  "foldMap" should {
    "be able to sum a list of ints" in {
      val list = List(1, 2, 3, 4)
      val sum = foldMap(list, intAddition)(identity)
      sum === list.sum
    }
  }

  "foldRight" should {
    "be able to build a list in the forward order" in {
      val list = List("1", "2", "3", "4")
      val newList = foldRight(list)(List.empty[Int]) { (a, b) =>
        a.toInt :: b
      }
      newList === (1 to 4).toList
    }
  }

  "foldLeft" should {
    "be able to build a list in reversed order" in {
      val list = List("1", "2", "3", "4")
      val newList = foldLeft(list)(List.empty[Int]) { (b, a) =>
        a.toInt :: b
      }
      newList === (1 to 4).reverse.toList
    }
  }

  "foldMapV" should {
    "be able to compute a product" in {
      val vector = (1 to 100).toVector
      val sum = foldMapV(vector, intMultiplication)(identity)
      sum === vector.product
    }
  }

  val es = Executors.newCachedThreadPool()

  override def afterAll() = es.shutdown()

  "parFoldMap" should {
    "be able to compute a product" in {
      val vector = (1 to 1000).toVector
      val pSum = parFoldMap(vector, intMultiplication)(identity)
      val sum = Par.run(es)(pSum)
      sum === vector.product
    }
  }

  "parFoldMap2" should {
    "be able to compute a product" in {
      val vector = (1 to 1000).toVector
      val pSum = parFoldMap2(vector, intMultiplication)(identity)
      val sum = Par.run(es)(pSum)
      sum === vector.product
    }
  }

  "ordered" should {
    "validate if a list of ordered" in {
      val sorted = (1 to 100) map (_ * 10) toVector

      ordered(sorted) should beTrue
    }
    "validate that a list is not ordered" in {
      val sorted = (1 to 100) map (_ * 10) toVector

      ordered(sorted.reverse) should beFalse
    }
    "validate an empty list as being ordered" in {
      ordered(Vector.empty) should beTrue
    }
  }

}
