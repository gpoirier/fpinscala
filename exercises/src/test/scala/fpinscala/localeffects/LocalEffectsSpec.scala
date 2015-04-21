package fpinscala.localeffects

import org.specs2.mutable._

class LocalEffectsSpec extends Specification {
  "STArray" >> {
    "fill" >> {

      val runnable = new RunnableST[(Int, Int)] {
        override def apply[S]: ST[S, (Int, Int)] = {
          for {
            a <- STArray(10, 1)
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

  "STMap" >> {
    "merge" >> {
      type Values = Option[(Int, Int, Int)]

      val runnable = new RunnableST[Values] {
        override def apply[S]: ST[S, Values] = {
          for {
            m <- STMap(10 -> 1, 12 -> 4)
            _ <- m.merge(Map(11 -> 2, 10 -> 3))
            first <- m.read(10)
            second <- m.read(11)
            third <- m.read(12)
          } yield for {
            i <- first
            j <- second
            k <- third
          } yield {
            (i, j, k)
          }
        }
      }

      ST.runST(runnable) should be some (3, 2, 4)

    }
  }

}
