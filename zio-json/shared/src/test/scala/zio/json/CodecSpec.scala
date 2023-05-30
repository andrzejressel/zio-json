package testzio.json

import zio._
import zio.json.ValidationAssertions.{ isSingleFailure, isSucceeded }
import zio.json._
import zio.json.ast.Json
import zio.prelude.Validation
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable

object CodecSpec extends ZIOSpecDefault {

  case class RecursiveOption(i: Int, d: Double, s: List[String], o: Option[RecursiveOption])
  object RecursiveOption {
    implicit lazy val codec: JsonCodec[RecursiveOption] = DeriveJsonCodec.gen[RecursiveOption]
  }

  val spec: Spec[Environment, Any] =
    suite("CodecSpec")(
      suite("Codec regressions")(
        test("option in recursive structure") {
          val expected = RecursiveOption(100, 0.25, List("a", "b"), Some(RecursiveOption(200, 0, Nil, None)))

          val decoded = JsonCodec[RecursiveOption].decoder
            .decodeJsonValidation("""{"i":100,"d":0.25,"s":["a","b"],"o":{"i":200,"d":0,"s":[]}}""")

          assertTrue(decoded.toEither.right.get == expected)
        }
      ),
      suite("Decoding")(
        test("empty") {
          import exampleempty._
          assert("{}".fromJsonValidation[Empty])(
            isSucceeded(equalTo(Empty(None)))
          )
        },
        test("primitives") {
          val exampleBDString = "234234.234"
          // this big integer consumes more than 128 bits
          assert("170141183460469231731687303715884105728".fromJsonValidation[java.math.BigInteger])(
            isSingleFailure(equalTo("(expected a 128 bit BigInteger)"))
          ) && assert(exampleBDString.fromJsonValidation[BigDecimal])(isSucceeded(equalTo(BigDecimal(exampleBDString))))
        },
        test("eithers") {
          val bernies = List("""{"a":1}""", """{"left":1}""", """{"Left":1}""")
          val trumps  = List("""{"b":2}""", """{"right":2}""", """{"Right":2}""")

          assert(bernies.map(_.fromJsonValidation[Either[Int, Int]]))(
            forall(isSucceeded(isLeft(equalTo(1))))
          ) && assert(trumps.map(_.fromJsonValidation[Either[Int, Int]]))(
            forall(isSucceeded(isRight(equalTo(2))))
          )
        },
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
        },
        test("key transformation") {
          import exampletransformkeys._
          val kebabed       = """{"shish123-kebab":""}"""
          val snaked        = """{"indiana123_jones":""}"""
          val pascaled      = """{"Anders123Hejlsberg":""}"""
          val cameled       = """{"small123Talk":""}"""
          val indianaJones  = """{"wHATcASEiStHIS":""}"""
          val overrides     = """{"not_modified":"","but-this-should-be":0}"""
          val kebabedLegacy = """{"shish-123-kebab":""}"""
          val snakedLegacy  = """{"indiana_123_jones":""}"""

          assert(kebabed.fromJsonValidation[Kebabed])(isSucceeded(equalTo(Kebabed("")))) &&
          assert(kebabedLegacy.fromJsonValidation[legacy.Kebabed])(isSucceeded(equalTo(legacy.Kebabed("")))) &&
          assert(snaked.fromJsonValidation[Snaked])(isSucceeded(equalTo(Snaked("")))) &&
          assert(snakedLegacy.fromJsonValidation[legacy.Snaked])(isSucceeded(equalTo(legacy.Snaked("")))) &&
          assert(pascaled.fromJsonValidation[Pascaled])(isSucceeded(equalTo(Pascaled("")))) &&
          assert(cameled.fromJsonValidation[Cameled])(isSucceeded(equalTo(Cameled("")))) &&
          assert(indianaJones.fromJsonValidation[Custom])(isSucceeded(equalTo(Custom("")))) &&
          assert(overrides.fromJsonValidation[OverridesAlsoWork])(isSucceeded(equalTo(OverridesAlsoWork("", 0)))) &&
          assertTrue(Kebabed("").toJson == kebabed) &&
          assertTrue(Kebabed("").toJsonAST.toOption.get == kebabed.fromJsonValidation[Json].toOption.get) &&
          assertTrue(legacy.Kebabed("").toJson == kebabedLegacy) &&
          assertTrue(legacy.Kebabed("").toJsonAST.toOption.get == kebabedLegacy.fromJsonValidation[Json].toOption.get) &&
          assertTrue(Snaked("").toJson == snaked) &&
          assertTrue(Snaked("").toJsonAST.toOption.get == snaked.fromJsonValidation[Json].toOption.get) &&
          assertTrue(legacy.Snaked("").toJson == snakedLegacy) &&
          assertTrue(legacy.Snaked("").toJsonAST.toOption.get == snakedLegacy.fromJsonValidation[Json].toOption.get) &&
          assertTrue(Pascaled("").toJson == pascaled) &&
          assertTrue(Pascaled("").toJsonAST.toOption.get == pascaled.fromJsonValidation[Json].toOption.get) &&
          assertTrue(Cameled("").toJson == cameled) &&
          assertTrue(Cameled("").toJsonAST.toOption.get == cameled.fromJsonValidation[Json].toOption.get) &&
          assertTrue(Custom("").toJson == indianaJones) &&
          assertTrue(Custom("").toJsonAST.toOption.get == indianaJones.fromJsonValidation[Json].toOption.get) &&
          assertTrue(OverridesAlsoWork("", 0).toJson == overrides)
        },
        test("unicode") {
          assert(""""€🐵🥰"""".fromJsonValidation[String])(isSucceeded(equalTo("€🐵🥰")))
        },
        test("Seq") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Seq("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[Seq[String]])(isSucceeded(equalTo(expected)))
        },
        test("Vector") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Vector("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[Vector[String]])(isSucceeded(equalTo(expected)))
        },
        test("SortedSet") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[immutable.SortedSet[String]])(isSucceeded(equalTo(expected)))
        },
        test("HashSet") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[immutable.HashSet[String]])(isSucceeded(equalTo(expected)))
        },
        test("Set") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Set("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[Set[String]])(isSucceeded(equalTo(expected)))
        },
        test("Map") {
          val jsonStr  = """{"5XL":3,"2XL":14,"XL":159}"""
          val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(jsonStr.fromJsonValidation[Map[String, Int]])(isSucceeded(equalTo(expected)))
        },
        test("zio.Chunk") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[Chunk[String]])(isSucceeded(equalTo(expected)))
        }
      ),
      suite("Encode -> Decode")(
        suite("control chars")(
          test("tab") {
            assert(encodeDecode(JsonCodec.char, '\t'))(isSucceeded(equalTo('\t')))
          },
          test("carriage return") {
            assert(encodeDecode(JsonCodec.char, '\r'))(isSucceeded(equalTo('\r')))
          },
          test("newline") {
            assert(encodeDecode(JsonCodec.char, '\n'))(isSucceeded(equalTo('\n')))
          },
          test("form feed") {
            assert(encodeDecode(JsonCodec.char, '\f'))(isSucceeded(equalTo('\f')))
          },
          test("backspace") {
            assert(encodeDecode(JsonCodec.char, '\b'))(isSucceeded(equalTo('\b')))
          },
          test("escape") {
            assert(encodeDecode(JsonCodec.char, '\\'))(isSucceeded(equalTo('\\')))
          },
          test("quote") {
            assert(encodeDecode(JsonCodec.char, '"'))(isSucceeded(equalTo('"')))
          }
        )
      )
    )

  private def encodeDecode[A](codec: JsonCodec[A], value: A): Validation[String, A] =
    codec.decodeJson(
      codec.encodeJson(value, None)
    )

  object exampleproducts {
    case class Parameterless()

    object Parameterless {
      implicit val codec: JsonCodec[Parameterless] = DeriveJsonCodec.gen[Parameterless]
    }

    @jsonNoExtraFields
    case class OnlyString(s: String)

    object OnlyString {
      implicit val codec: JsonCodec[OnlyString] = DeriveJsonCodec.gen[OnlyString]
    }
  }

  object examplesum {
    sealed abstract class Parent

    object Parent {
      implicit val codec: JsonCodec[Parent] = DeriveJsonCodec.gen[Parent]
    }
    case class Child1() extends Parent
    case class Child2() extends Parent
  }

  object exampleempty {
    case class Empty(a: Option[String])

    object Empty {
      implicit val codec: JsonCodec[Empty] = DeriveJsonCodec.gen[Empty]
    }
  }

  object examplealtsum {

    @jsonDiscriminator("hint")
    sealed abstract class Parent

    object Parent {
      implicit val codec: JsonCodec[Parent] = DeriveJsonCodec.gen[Parent]
    }

    @jsonHint("Cain")
    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent
  }

  object exampletransformkeys {
    @jsonMemberNames(KebabCase)
    case class Kebabed(shish123Kebab: String)
    object Kebabed {
      implicit val codec: JsonCodec[Kebabed] = DeriveJsonCodec.gen[Kebabed]
    }

    @jsonMemberNames(SnakeCase)
    case class Snaked(indiana123Jones: String)
    object Snaked {
      implicit val codec: JsonCodec[Snaked] = DeriveJsonCodec.gen[Snaked]
    }

    @jsonMemberNames(PascalCase)
    case class Pascaled(anders123Hejlsberg: String)
    object Pascaled {
      implicit val codec: JsonCodec[Pascaled] = DeriveJsonCodec.gen[Pascaled]
    }

    @jsonMemberNames(CamelCase)
    case class Cameled(small123_talk: String)
    object Cameled {
      implicit val codec: JsonCodec[Cameled] = DeriveJsonCodec.gen[Cameled]
    }

    @jsonMemberNames(CustomCase(Custom.indianaJonesCase))
    case class Custom(whatCaseIsThis: String)
    object Custom {
      def indianaJonesCase(str: String): String =
        str
          .split("(?=\\p{Upper})")
          .map(part => s"${part.head.toLower}${part.substring(1, part.length).toUpperCase}")
          .mkString

      implicit val codec: JsonCodec[Custom] = DeriveJsonCodec.gen[Custom]
    }

    @jsonMemberNames(KebabCase)
    case class OverridesAlsoWork(@jsonField("not_modified") notModified: String, butThisShouldBe: Int)
    object OverridesAlsoWork {
      implicit val codec: JsonCodec[OverridesAlsoWork] = DeriveJsonCodec.gen[OverridesAlsoWork]
    }

    object legacy {
      @jsonMemberNames(ziojson_03.KebabCase)
      case class Kebabed(shish123Kebab: String)

      object Kebabed {
        implicit val codec: JsonCodec[Kebabed] = DeriveJsonCodec.gen[Kebabed]
      }

      @jsonMemberNames(ziojson_03.SnakeCase)
      case class Snaked(indiana123Jones: String)

      object Snaked {
        implicit val codec: JsonCodec[Snaked] = DeriveJsonCodec.gen[Snaked]
      }

    }
  }

  object logEvent {
    case class Event(at: Long, message: String, a: Seq[String] = Nil)
    implicit val codec: JsonCodec[Event] = DeriveJsonCodec.gen[Event]
  }
}
