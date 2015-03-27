package fpinscala.monads

import org.specs2.mutable._

import fpinscala.state._

class MonadSpec extends Specification {
  "sequence" should {
    "transform a list of Option to an option of a list" in {
      val M = Monad.optionMonad
      val list = M.sequence(List(Option(1), Option(3)))
      list == Option(List(1, 3))

      M.sequence(List(Option(1), None)) === None
    }
  }

  "replicateM" should {
    "replicate a list" in {
      val M = Monad.listMonad
      val x = M.replicateM(3, List(1, 2))
      x === List(
        List(1, 1, 1),
        List(1, 1, 2),
        List(1, 2, 1),
        List(1, 2, 2),
        List(2, 1, 1),
        List(2, 1, 2),
        List(2, 2, 1),
        List(2, 2, 2)
      )
    }

    "replicate an option" in {
      val M = Monad.optionMonad
      M.replicateM(10, Some(1)) === Some(List.fill(10)(1))
      M.replicateM(10, None) === None
    }
  }

  "filterM" in {
    "filter with a list" in {
      val M = Monad.listMonad
      val filtered = M.filterM(List(1, 2, 3)) { n =>
        (1 to n).map(_ % 2 == 0).toList
      }
      filtered === List(Nil, List(3), Nil, List(2), List(2, 3), List(2))
    }
    "filter with an option" in {
      val M = Monad.optionMonad

      val filtered1 = M.filterM((1 to 10).toList) { n =>
        Option(n % 2 == 0)
      }
      filtered1 === Some(List(2, 4, 6, 8, 10))

      val filtered2 = M.filterM((1 to 10).map(_ * 2).toList) { n =>
        Option(n % 2 == 1)
      }
      filtered2 === Some(Nil)

      val filtered3 = M.filterM((1 to 10).map(_ * 2).toList) { _ => None }
      filtered3 === None
    }
  }

  "stateMonad" >> {
    val M = Monad.stateMonad[Int]

    "replicateM" >> {
      val list = M.replicateM(10, State { i => (i % 3 == 0) -> (i + 1) })
      list.run(0) === (0 until 10).map(_ % 3 == 0).toList -> 10
      list.run(3) === (3 until 13).map(_ % 3 == 0).toList -> 13
    }

    "map2" >> {
      val s1 = State[Int, Boolean] { i => (i % 2 == 0) -> (i + 1) }
      val s2 = State[Int, Boolean] { i => (i % 3 == 0) -> (i + 1) }
      val s3 = M.map2(s1, s2)(_ && _)

      for {
        (init, expected) <- Map(
          0 -> false,
          1 -> false,
          2 -> true, // 2 % 2 == 0 && 3 % 3 == 0
          3 -> false,
          5 -> false,
          6 -> false,
          8 -> true, // 8 % 2 == 0 && 9 % 3 == 0
          10 -> false
        )
      } yield {
        s3.run(init)._1 aka s"run($init)" should be_==(expected)
      }
      success
    }
  }
}
