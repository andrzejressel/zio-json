package testzio.json

import zio.json._
import zio.prelude.Validation
import zio.prelude.ZValidation.succeed
import zio.test.Assertion._
import zio.test._

object CarterSpec extends ZIOSpecDefault {

  case class Union[A, B](extract: Either[A, B])
  object Union {
    implicit def decoder[A, B](implicit A: JsonDecoder[A], B: JsonDecoder[B]): JsonDecoder[Union[A, B]] =
      A.orElseEither(B).map(Union(_))
  }

  case class Testing0(
    y: Union[String, Int]
  )
  object Testing0 {
    implicit val decoder: JsonDecoder[Testing0] = DeriveJsonDecoder.gen
  }

  case class Testing1(
    x: Union[String, Double],
    y: Union[String, Int]
  )
  object Testing1 {
    implicit val decoder: JsonDecoder[Testing1] = DeriveJsonDecoder.gen
  }

  val spec: Spec[Environment, Any] =
    suite("Carter")(
      test("simple left") {
        type Data = Union[String, Int]
        val expect: Validation[String, Data] = succeed(Union(Left("foo")))
        assert(JsonDecoder[Data].decodeJsonValidation("\"foo\""))(equalTo(expect))
      },
      test("simple right") {
        type Data = Union[String, Int]
        val expect: Validation[String, Data] = succeed(Union(Right(1)))
        assert(JsonDecoder[Data].decodeJsonValidation("1"))(equalTo(expect))
      },
      test("case class 1 field") {
        val expect: Validation[String, Testing0] = succeed(Testing0(Union(Left("2025-01-01"))))
        assert(JsonDecoder[Testing0].decodeJsonValidation("{\"y\":\"2025-01-01\"}"))(equalTo(expect))
      },
      test("https://github.com/zio/zio-json/issues/209") {
        // that works fine, normally, but we wrap the input reader with a
        // RecordingReader and that doesn't tell the underlying thing to
        // retract. there are two possible fixes:
        //
        // 1. the RecordingReader should .retract the underlying thing when it's
        //    "caught up" (which is problematic because the thing underneath is
        //    not necessarily capable of retracting... abstraction bug)
        //
        // 2. or (uuugh) the caller should always check if a recording reader is
        //    all caught up before returning control back to the underlying
        val expect: Validation[String, Testing1] = succeed(Testing1(Union(Right(0.1)), Union(Left("2025-01-01"))))
        assert(JsonDecoder[Testing1].decodeJsonValidation("{\"x\":0.1,\"y\":\"2025-01-01\"}"))(equalTo(expect))
      },
      test("whitespace showing no retract call") {
        val expect: Validation[String, Testing1] = succeed(Testing1(Union(Right(0.1)), Union(Left("2025-01-01"))))
        assert(JsonDecoder[Testing1].decodeJsonValidation("{\"x\":0.1 ,\"y\":\"2025-01-01\"}"))(equalTo(expect))
      }
    )

}
