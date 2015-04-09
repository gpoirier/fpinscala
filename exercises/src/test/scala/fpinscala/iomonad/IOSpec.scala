package fpinscala.iomonad

import org.specs2.mutable._
import org.specs2.matcher.Scope

class IOSpec extends Specification {

  class IO3Fixture extends Scope {
    import IO3._
    import Console._

    def countDown(from: Int): Free[Console, Unit] = {
      if (from > 0)
        for {
          _ <- printLn(s"$from...")
          _ <- countDown(from - 1)
        } yield ()
      else
        printLn("0.")
    }
  }

  "runConsoleFunction0" >> {
    "is not stack-safe" >> new IO3Fixture {
      import IO3._

      runConsoleFunction0(countDown(100000)).apply should throwAn[StackOverflowError]
    }
  }

  "runConsole" >> {
    "is stacksafe" >> new IO3Fixture {
      import IO3._

      try {
        runConsole(countDown(100000)) should beEqualTo(())
      } catch {
        case e: StackOverflowError => failure("Stack Overflow")
      }
    }
  }
}
