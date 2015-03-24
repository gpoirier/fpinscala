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

  val lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit"


  "WC" should {
    "count the words in lorem ipsum" in {
      Stub(lorem).count === 8
    }

    "count zero for empty string" in {
      Stub(", ").count === 0
      Stub(" ").count === 0
      Stub("").count === 0
    }
  }

  "count" should {
    "count the number of word in a short lorem ipsum" in {

      implicit val m = wcMonoid

      m.op(Stub("Lorem "), Stub("ipsum d")) === Part("Lorem ", 1, " d")
      m.op(Stub("olor si"), Stub("t amet,")) === Part("olor ", 2, ",")

      m.op(Stub(" consec"), Stub("tetur a")) === Part(" ", 1, " a")
      m.op(Stub("dipisci"), Stub("ng elit")) === Part("dipiscing ", 0, " elit")

      m.op(Part("Lorem ", 1, " d"), Part("olor ", 2, ",")) == Part("Lorem ", 4, ",")
      m.op(Part(" ", 1, " a"), Part("dipiscing ", 0, " elit")) == Part(" ", 2, " elit")

      m.op(Part("Lorem ", 4, ","), Part(" ", 2, " elit")) == Part("Lorem ", 6, "elit")

      count(lorem) === 8
    }
  }

  verifyLaws("productMonoid #1", productMonoid(intAddition, intMultiplication)) {
    for {
      i <- Gen.int
      j <- Gen.int
    } yield i -> j
  }

  verifyLaws("productMonoid #2", productMonoid(booleanAnd, booleanOr)) {
    for {
      b1 <- Gen.boolean
      b2 <- Gen.boolean
    } yield b1 -> b2
  }

  "mapMergeMonoid" should {
    "merge two maps using intAddition monoid" in {
      val m = mapMergeMonoid[String, Int](intAddition)

      val merged = m.op(Map("abc" -> 10, "def" -> 20), Map("def" -> 5, "xyz" -> 11))
      merged === Map("abc" -> 10, "def" -> 25, "xyz" -> 11)
    }
  }

  "bag" should {
    "create a map out of a vector" in {
      val vector = Vector("abc", "def", "xyz", "def", "qwe", "xyz")
      bag(vector) === Map("abc" -> 1, "def" ->2, "xyz" -> 2, "qwe" -> 1)
      bag_groupBy(vector) === Map("abc" -> 1, "def" ->2, "xyz" -> 2, "qwe" -> 1)
    }
  }
}
