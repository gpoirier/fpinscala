package fpinscala.localeffects

import org.specs2.mutable._

class LocalEffectsSpec extends Specification {
  "STArray" >> {
    "fill" >> {

      val runnable = new RunnableST[(Int, Int)] {
        override def apply[S]: ST[S, (Int, Int)] = {
          for {
            a <- STArray.apply(10, 1: Int)
            _ <- a.fill(Map(0 -> 5, 5 -> 6))
            first <- a.read(0)
            second <- a.read(1)
          } yield {
            (first, second)
          }
        }
      }

      ST.runST(runnable) === (5, 1)
    }
  }

  "Immutable" >> {
    "quicksort" >> {
      def test(xs: List[Int]) = {
        Immutable.quicksort(xs) === xs.sorted
      }

      for {
        ints <- (1 to 6).permutations
      } test(ints.toList)

      success
    }
  }
}
