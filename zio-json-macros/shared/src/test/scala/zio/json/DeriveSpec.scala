package testzio.json

import zio.json.ValidationAssertions.{ isSingleFailure, isSucceeded }
import zio.json._
import zio.test.Assertion._
import zio.test._

object DeriveSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("DeriveCodec")(
      suite("Decoding")(
        test("parameterless products") {
          import exampleproducts._

          // actually anything works... consider this a canary test because if only
          // the empty object is supported that's fine.
          assert("""{}""".fromJsonValidation[Parameterless])(isSucceeded(equalTo(Parameterless()))) &&
          assert("""null""".fromJsonValidation[Parameterless])(isSucceeded(equalTo(Parameterless()))) &&
          assert("""{"field":"value"}""".fromJsonValidation[Parameterless])(isSucceeded(equalTo(Parameterless())))
        },
        test("no extra fields") {
          import exampleproducts._

          assert("""{"s":""}""".fromJsonValidation[OnlyString])(isSucceeded(equalTo(OnlyString("")))) &&
          assert("""{"s":"","t":""}""".fromJsonValidation[OnlyString])(isSingleFailure(equalTo("(invalid extra field)")))
        },
        test("sum encoding") {
          import examplesum._

          assert("""{"Child1":{}}""".fromJsonValidation[Parent])(isSucceeded(equalTo(Child1()))) &&
          assert("""{"Child2":{}}""".fromJsonValidation[Parent])(isSucceeded(equalTo(Child2()))) &&
          assert("""{"type":"Child1"}""".fromJsonValidation[Parent])(isSingleFailure(equalTo("(invalid disambiguator)")))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert("""{"hint":"Cain"}""".fromJsonValidation[Parent])(isSucceeded(equalTo(Child1()))) &&
          assert("""{"hint":"Abel"}""".fromJsonValidation[Parent])(isSucceeded(equalTo(Child2()))) &&
          assert("""{"hint":"Samson"}""".fromJsonValidation[Parent])(isSingleFailure(equalTo("(invalid disambiguator)"))) &&
          assert("""{"Cain":{}}""".fromJsonValidation[Parent])(isSingleFailure(equalTo("(missing hint 'hint')")))
        }
      )
    )

  object exampleproducts {
    @jsonDerive
    case class Parameterless()

    @jsonDerive
    @jsonNoExtraFields
    case class OnlyString(s: String)
  }

  object examplesum {
    @jsonDerive
    sealed abstract class Parent

    case class Child1() extends Parent
    case class Child2() extends Parent
  }

  object exampleempty {
    @jsonDerive
    case class Empty(a: Option[String])

  }

  object examplealtsum {

    @jsonDerive
    @jsonDiscriminator("hint")
    sealed abstract class Parent

    @jsonHint("Cain")
    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent
  }

  object logEvent {
    @jsonDerive(JsonDeriveConfig.Decoder)
    case class Event(at: Long, message: String, a: Seq[String] = Nil)
  }

}
