package fpinscala.streamingio

import org.specs2.mutable._

class StreamingIOSpec extends Specification {

  "SimpleStreamTransducers" >> {

    import SimpleStreamTransducers._

    "take all" >> {
      val p: Stream[Int] = Process.take(10)(Stream.from(5))
      p.toList === (5 until 15).toList
    }
    "take short" >> {
      val p: Stream[Int] = Process.take(10)(Stream.range(5, 8))
      p.toList === (5 until 8).toList
    }
    "take empty" >> {
      val p: Stream[Int] = Process.take(10)(Stream.empty)
      p === Stream.empty
    }
    "drop all" >> {
      val p: Stream[Int] = Process.drop(10)(Stream.from(5))
      p.take(5).toList === (15 until 20).toList
    }
    "drop short" >> {
      val p: Stream[Int] = Process.drop(10)(Stream.range(5, 10))
      p.toList === Nil
    }
    "drop empty" >> {
      val p: Stream[Int] = Process.drop(10)(Stream.empty)
      p.toList === Nil
    }

    "takeWhile with matches" >> {
      val p: Stream[Int] = Process.takeWhile[Int](_ < 10)(Stream.from(5))
      p.toList === (5 until 10).toList
    }
    "takeWhile without match" >> {
      val p: Stream[Int] = Process.takeWhile[Int](_ < 10)(Stream.from(15))
      p === Stream.empty
    }
    "takeWhile from empty" >> {
      val p: Stream[Int] = Process.takeWhile[Int](_ < 10)(Stream.empty)
      p === Stream.empty
    }

    "dropWhile with some matches" >> {
      val p: Stream[Int] = Process.dropWhile[Int](_ < 10)(Stream.from(5))
      p.take(10).toList === (10 until 20).toList
    }
    "dropWhile with all matches" >> {
      val p: Stream[Int] = Process.dropWhile[Int](_ < 10)(Stream.continually(15).take(100))
      p.toList === List.fill(100)(15)
    }
    "dropWhile without match" >> {
      val p: Stream[Int] = Process.dropWhile[Int](_ < 10)(Stream.from(15).take(10))
      p.toList === (15 until 25).toList
    }
    "dropWhile from empty" >> {
      val p: Stream[Int] = Process.dropWhile[Int](_ < 10)(Stream.empty)
      p === Stream.empty
    }

    "count empty" >> {
      Process.count(Stream.empty) === Stream()
    }
    "count 1" >> {
      Process.count(Stream(10)) === Stream(1)
    }
    "count 2" >> {
      Process.count(Stream(10, 20)) === Stream(1, 2)
    }
    "count 100" >> {
      Process.count(Stream.from(100, 40).take(100)) === Stream.from(1).take(100)
    }

    "mean" >> {
      Process.mean(Stream.range(10, 21, 2).map(_.toDouble)) === Stream(10, 11, 12, 13, 14, 15).map(_.toDouble)
    }

    "sum2" >> {
      val expected = Stream.range(0, 100).map(i => (0 to i).toList.sum.toDouble)
      Process.sum2(Stream.range(0, 100).map(_.toDouble)) === expected
    }

    "count3 empty" >> {
      Process.count3(Stream.empty) === Stream()
    }
    "count3 1" >> {
      Process.count3(Stream(10)) === Stream(1)
    }
    "count3 2" >> {
      Process.count3(Stream(10, 20)) === Stream(1, 2)
    }
    "count3 100" >> {
      Process.count3(Stream.from(100, 40).take(100)) === Stream.from(1).take(100)
    }

    "sum |> mean" >> {

      val numbers = Stream.from(0).map(_.toDouble)

      // sum => (0, 1, 3, 6, 10)
      // avg => (0, 0.5, 4/3, 9/4, 20/5)

      val composite = (Process.sum |> Process.mean)(numbers)

      composite.take(5) === Stream(0.0, 0.5, 4.0 / 3, 10.0 / 4, 20.0 / 5)
    }

    "filter |> (* 2)" >> {
      val numbers = Stream.from(0)

      val p = Process.filter((_: Int) % 2 == 0) |> Process.lift(_ * 2)

      p(numbers).take(5) === Stream(0, 4, 8, 12, 16)
    }

    "Process#zipWithIndex" >> {
      val p = Process.filter((_: Int) % 2 == 0).zipWithIndex
      p(Stream.from(0)).take(3) === Stream(0 -> 0, 2 -> 1, 4 -> 2)
    }
    "Process.zipWithIndex" >> {
      val p = Process.zipWithIndex[Int]
      p(Stream.from(10)).take(3) === Stream(10 -> 0, 11 -> 1, 12 -> 2)
    }

    "mean2" >> {
      Process.mean2(Stream.range(10, 21, 2).map(_.toDouble)) === Stream(10, 11, 12, 13, 14, 15).map(_.toDouble)
    }
    "mean3" >> {
      Process.mean3(Stream.range(10, 21, 2).map(_.toDouble)) === Stream(10, 11, 12, 13, 14, 15).map(_.toDouble)
    }

    "exist" >> {
      val p = Process.exists[Int](_ % 2 == 0)
      p(Stream(1, 3, 5, 6, 7, 4)) === Stream(false, false, false, true, true, true)
    }

    "convertFahrenheit" >> {
      val input = "32" #:: " " #:: "# Comment" #:: "77" #:: Stream.empty

      Process.convertFahrenheit(input) === "0.0" #:: "25.0" #:: Stream.empty
    }
  }

  "GeneralizedStreamTransducers" >> {
    import GeneralizedStreamTransducers._

    "runLog" >> {
      pending
    }
  }
}
