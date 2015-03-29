package fpinscala.applicative

import org.specs2.mutable._

import Applicative._
import org.specs2.specification.Scope

import scala.util.Try
import scala.util.control.NonFatal

class ApplicativeSpec extends Specification {

  "streamApplicative" >> {
    "sequence" >> {

      skipped("sequences cause a endless loop because the stream is infinite")

      val A = streamApplicative
      val streams = (1 to 10).map(i => A.unit(i)).toList
      A.sequence(streams) === Stream(???)
    }
  }

  class EitherFixture extends Scope {
    import java.util.Date
    val fmt = new java.text.SimpleDateFormat("yyyy-MM-dd")

    case class WebForm(name: String, birthDate: Date, phone: Int)

    def validateName(text: String) =
      if (text == "Guillaume") Right(text)
      else Left(s"Unexpected name: $text")

    def validateBirthDate(text: String): Either[String, Date] =
      try {
        Right(fmt.parse(text))
      } catch {
        case NonFatal(e) => Left(e.getMessage)
      }

    def validatePhone(text: String): Either[String, Int] =
      if (text.matches(raw"\d{3}-\d{3}-\d{4}"))
        Right(text.replaceAll("-", "").toInt)
      else
        Left(s"Phone doesn't match expected format (XXX-XXX-XXXX): $text")
  }

  "eitherMonad" >> {
    "flatMap" >> new EitherFixture {
      val form = for {
        name <- validateName("Guillaume").right
        birthDate <- validateBirthDate("1980/04/16").right
        phone <- validatePhone("1-514-111-1111").right
      } yield WebForm(name, birthDate, phone)

      form === validateBirthDate("1980/04/16")
    }

    "map3" >> new EitherFixture {
      val form = Monad.eitherMonad.map3(
        validateName("Guillaume"),
        validateBirthDate("1980/04/16"),
        validatePhone("1-514-111-1111")
      ) {
        WebForm.apply
      }

      form === validateBirthDate("1980/04/16")
    }
  }

  "eitherApplicative" >> {
    "map3" >> {

      import java.util.Date
      val fmt = new java.text.SimpleDateFormat("yyyy-MM-dd")

      case class WebForm(name: String, birthDate: Date, phone: Int)

      def validateName(text: String): Validation[String, String] =
        if (text == "Guillaume") Success(text)
        else Success(s"Unexpected name: $text")

      def validateBirthDate(text: String): Validation[String, Date] =
        try {
          Success(fmt.parse(text))
        } catch {
          case NonFatal(e) => Failure(e.getMessage)
        }

      def validatePhone(text: String): Validation[String, Int] =
        if (text.matches(raw"\d{3}-\d{3}-\d{4}"))
          Success(text.replaceAll("-", "").toInt)
        else
          Failure(s"Phone doesn't match expected format (XXX-XXX-XXXX): $text")

      val form = Applicative.validationApplicative.map3(
        validateName("Guillaume"),
        validateBirthDate("1980/04/16"),
        validatePhone("1-514-111-1111")
      ) {
        WebForm.apply
      }

      form === Failure(
        """Unparseable date: "1980/04/16"""",
        Vector("Phone doesn't match expected format (XXX-XXX-XXXX): 1-514-111-1111"))

    }
  }

  "eitherMonad" >> {
    "sequenceMap" >> {
      val M = Monad.eitherMonad[String]

      val rightMap: Map[String, Either[String, Int]] = Map("one" -> Right(1), "two" -> Right(2))

      M.sequenceMap(rightMap) === Right(Map("one" -> 1, "two" -> 2))

      val mixedMap = rightMap + ("tree" -> Left("NaN"))

      M.sequenceMap(mixedMap) === Left("NaN")
    }
  }
}
