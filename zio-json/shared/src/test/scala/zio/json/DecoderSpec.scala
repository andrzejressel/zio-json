package testzio.json

import zio._
import zio.json.ValidationAssertions.{isDoubleFailure, isSingleFailure, isSucceeded}
import zio.json._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

import java.time.{Duration, OffsetDateTime, ZonedDateTime}
import java.util.UUID
import scala.collection.{SortedMap, immutable, mutable}

object DecoderSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("Decoder")(
      suite("fromJson")(
        test("BigDecimal") {
          assert("123".fromJsonValidation[BigDecimal])(isSucceeded(equalTo(BigDecimal(123))))
        },
        test("BigInteger too large") {
          // this big integer consumes more than 128 bits
          assert("170141183460469231731687303715884105728".fromJsonValidation[java.math.BigInteger])(
            isSingleFailure(equalTo("(expected a 128 bit BigInteger)"))
          )
        },
        test("collections") {
          val arr = """[1, 2, 3]"""
          val obj = """{ "a": 1 }"""

          assert(arr.fromJsonValidation[Array[Int]])(isSucceeded(equalTo(Array(1, 2, 3)))) &&
          assert(arr.fromJsonValidation[IndexedSeq[Int]])(isSucceeded(equalTo(IndexedSeq(1, 2, 3)))) &&
          assert(arr.fromJsonValidation[immutable.LinearSeq[Int]])(isSucceeded(equalTo(immutable.LinearSeq(1, 2, 3)))) &&
          assert(arr.fromJsonValidation[immutable.ListSet[Int]])(isSucceeded(equalTo(immutable.ListSet(1, 2, 3)))) &&
          assert(arr.fromJsonValidation[immutable.TreeSet[Int]])(isSucceeded(equalTo(immutable.TreeSet(1, 2, 3)))) &&
          assert(obj.fromJsonValidation[mutable.Map[String, Int]])(isSucceeded(equalTo(mutable.Map("a" -> 1))))
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
        test("typical") {
          case class Banana(ripe: Boolean, curvature: Double)
          implicit val decoder: JsonDecoder[Banana] = DeriveJsonDecoder.gen

          assert("""{"curvature": 7, "ripe": true}""".fromJsonValidation[Banana])(
            isSucceeded(
              equalTo(Banana(curvature = 7, ripe = true))
            )
          )
        },
        test("no extra fields") {
          import exampleproducts._

          assert("""{"s":""}""".fromJsonValidation[OnlyString])(isSucceeded(equalTo(OnlyString("")))) &&
          assert("""{"s":"","t":""}""".fromJsonValidation[OnlyString])(isSingleFailure(equalTo("(invalid extra field)")))
        },
        test("aliases") {
          case class Apple(@jsonAliases("ripeness", "old") ripe: Boolean, taste: Double)
          implicit val decoder: JsonDecoder[Apple] = DeriveJsonDecoder.gen

          val expected = Apple(ripe = true, taste = 7)
          assert("""{"taste":7,"ripe":true}""".fromJsonValidation[Apple])(isSucceeded(equalTo(expected))) &&
          assert("""{"taste":7,"ripeness":true}""".fromJsonValidation[Apple])(isSucceeded(equalTo(expected))) &&
          assert("""{"taste":7,"old":true}""".fromJsonValidation[Apple])(isSucceeded(equalTo(expected))) &&
          assert("""{"taste":1,"ripe":true,"old":true}""".fromJsonValidation[Apple])(isSingleFailure(equalTo("(duplicate)"))) &&
          assert("""{"taste":1,"ripeness":true,"old":true}""".fromJsonValidation[Apple])(isSingleFailure(equalTo("(duplicate)")))
        },
        test("aliases - alias collides with field name") {
          for {
            error <- ZIO.attempt {
                       case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("radius") r: Int)
                       DeriveJsonDecoder.gen[Mango]
                     }.flip
          } yield assertTrue(
            // Class name in Scala 2: testzio.json.DecoderSpec.spec.Mango
            // Class name in Scala 3: testzio.json.DecoderSpec.spec.$anonfun.Mango
            error.getMessage.matches(
              "Field names and aliases in case class testzio.json.DecoderSpec.spec(.\\$anonfun)?.Mango must be distinct, alias\\(es\\) r collide with a field or another alias"
            )
          )
        },
        test("aliases - alias collides with another alias") {
          for {
            error <- ZIO.attempt {
                       case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("r") radius: Int)
                       DeriveJsonDecoder.gen[Mango]
                     }.flip
          } yield assertTrue(
            error.getMessage.matches(
              "Field names and aliases in case class testzio.json.DecoderSpec.spec(.\\$anonfun)?.Mango must be distinct, alias\\(es\\) r collide with a field or another alias"
            )
          )
        },
        test("aliases - double alias") {
          for {
            error <- ZIO.attempt {
                       case class Mango(@jsonAliases("r", "r") roundness: Int, radius: Int)
                       DeriveJsonDecoder.gen[Mango]
                     }.flip
          } yield assertTrue(
            error.getMessage.matches(
              "Field names and aliases in case class testzio.json.DecoderSpec.spec(.\\$anonfun)?.Mango must be distinct, alias\\(es\\) r collide with a field or another alias"
            )
          )
        },
        test("option") {
          case class WithOpt(id: Int, opt: Option[Int])
          implicit val decoder: JsonDecoder[WithOpt] = DeriveJsonDecoder.gen

          assert("""{ "id": 1, "opt": 42 }""".fromJsonValidation[WithOpt])(isSucceeded(equalTo(WithOpt(1, Some(42))))) &&
          assert("""{ "id": 1 }""".fromJsonValidation[WithOpt])(isSucceeded(equalTo(WithOpt(1, None))))
        },
        test("option - fromJsonAST") {
          case class WithOpt(id: Int, opt: Option[Int])
          implicit val decoder: JsonDecoder[WithOpt] = DeriveJsonDecoder.gen

          assert("""{ "id": 1, "opt": 42 }""".fromJsonValidation[Json].flatMap(decoder.fromJsonAST))(
            isSucceeded(equalTo(WithOpt(1, Some(42))))
          ) &&
          assert("""{ "id": 1 }""".fromJsonValidation[Json].flatMap(decoder.fromJsonAST))(isSucceeded(equalTo(WithOpt(1, None))))
        },
        test("default field value") {
          import exampleproducts._

          assert("""{}""".fromJsonValidation[DefaultString])(isSucceeded(equalTo(DefaultString("")))) &&
          assert("""{"s": null}""".fromJsonValidation[DefaultString])(isSucceeded(equalTo(DefaultString(""))))
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
        test("unicode") {
          assert(""""€🐵🥰"""".fromJsonValidation[String])(isSucceeded(equalTo("€🐵🥰")))
        },
        test("Option: .map on derived JsonDecoder with missing value") {
          // More information about use case here https://github.com/zio/zio-json/issues/198
          // User wants to derive an alternative encoding of optionality
          sealed trait Assumed[+A]

          object Assumed {
            case object MissingAssumed       extends Assumed[Nothing]
            case class FoundAssumed[A](v: A) extends Assumed[A]

            implicit def decoder[A](implicit decoding: JsonDecoder[A]): JsonDecoder[Assumed[A]] =
              JsonDecoder.option[A].map {
                case None    => Assumed.MissingAssumed
                case Some(v) => Assumed.FoundAssumed[A](v)
              }
          }

          case class Example(a: Assumed[Boolean])
          implicit val exampleDecoder: JsonDecoder[Example] = DeriveJsonDecoder.gen[Example]

          assert("""{ "a": null }""".fromJsonValidation[Example])(isSucceeded(equalTo(Example(Assumed.MissingAssumed)))) &&
          assert("""{ "a": true }""".fromJsonValidation[Example])(isSucceeded(equalTo(Example(Assumed.FoundAssumed(true))))) &&
          assert("""{ "a": false }""".fromJsonValidation[Example])(isSucceeded(equalTo(Example(Assumed.FoundAssumed(false))))) &&
          assert("""{ }""".fromJsonValidation[Example])(isSucceeded(equalTo(Example(Assumed.MissingAssumed))))
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
        test("Map with unicode keys") {
          val expected = Map(new String(Array('\u0007', '\n')) -> "value")
          val jsonStr  = JsonEncoder[Map[String, String]].encodeJson(expected, None)
          assert(jsonStr.fromJsonValidation[Map[String, String]])(isSucceeded(equalTo(expected)))
        },
        test("zio.Chunk") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[Chunk[String]])(isSucceeded(equalTo(expected)))
        },
        test("zio.NonEmptyChunk") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assert(jsonStr.fromJsonValidation[NonEmptyChunk[String]])(isSucceeded(equalTo(expected)))
        },
        test("zio.NonEmptyChunk failure") {
          val jsonStr = "[]"

          assert(jsonStr.fromJsonValidation[NonEmptyChunk[String]])(isSingleFailure(equalTo("(Chunk was empty)")))
        },
        test("java.util.UUID") {
          val ok1  = """"64d7c38d-2afd-4514-9832-4e70afe4b0f8""""
          val ok2  = """"0000000064D7C38D-FD-14-32-70AFE4B0f8""""
          val ok3  = """"0-0-0-0-0""""
          val bad1 = """"""""
          val bad2 = """"64d7c38d-2afd-4514-9832-4e70afe4b0f80""""
          val bad3 = """"64d7c38d-2afd-4514-983-4e70afe4b0f80""""
          val bad4 = """"64d7c38d-2afd--9832-4e70afe4b0f8""""
          val bad5 = """"64d7c38d-2afd-XXXX-9832-4e70afe4b0f8""""
          val bad6 = """"64d7c38d-2afd-X-9832-4e70afe4b0f8""""
          val bad7 = """"0-0-0-0-00000000000000000""""

          assert(ok1.fromJsonValidation[UUID])(isSucceeded(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))) &&
          assert(ok2.fromJsonValidation[UUID])(isSucceeded(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AfE4B0f8")))) &&
          assert(ok3.fromJsonValidation[UUID])(isSucceeded(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))) &&
          assert(bad1.fromJsonValidation[UUID])(isSingleFailure(containsString("Invalid UUID: "))) &&
          assert(bad2.fromJsonValidation[UUID])(isSingleFailure(containsString("Invalid UUID: UUID string too large"))) &&
          assert(bad3.fromJsonValidation[UUID])(
            isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd-4514-983-4e70afe4b0f80"))
          ) &&
          assert(bad4.fromJsonValidation[UUID])(
            isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd--9832-4e70afe4b0f8"))
          ) &&
          assert(bad5.fromJsonValidation[UUID])(
            isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"))
          ) &&
          assert(bad6.fromJsonValidation[UUID])(
            isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd-X-9832-4e70afe4b0f8"))
          ) &&
          assert(bad7.fromJsonValidation[UUID])(isSingleFailure(containsString("Invalid UUID: 0-0-0-0-00000000000000000")))
        },
        test("java.time.Duration") {
          val ok1  = """"PT1H2M3S""""
          val ok2  = """"PT-0.5S"""" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
          val bad1 = """"PT-H""""

          assert(ok1.fromJsonValidation[Duration])(isSucceeded(equalTo(Duration.parse("PT1H2M3S")))) &&
          assert(ok2.fromJsonValidation[Duration])(isSucceeded(equalTo(Duration.ofNanos(-500000000)))) &&
          assert(bad1.fromJsonValidation[Duration])(
            isSingleFailure(containsString("PT-H is not a valid ISO-8601 format, expected digit at index 3"))
          )
        },
        test("java.time.ZonedDateTime") {
          val ok1 = """"2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]""""
          val ok2 =
            """"2018-10-28T02:30+00:00[Europe/Warsaw]"""" // see https://bugs.openjdk.java.net/browse/JDK-8066982
          val bad1 = """"2018-10-28T02:30""""

          assert(ok1.fromJsonValidation[ZonedDateTime])(
            isSucceeded(equalTo(ZonedDateTime.parse("2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]")))
          ) &&
          assert(ok2.fromJsonValidation[ZonedDateTime].map(_.toOffsetDateTime))(
            isSucceeded(equalTo(OffsetDateTime.parse("2018-10-28T03:30+01:00")))
          ) &&
          assert(bad1.fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2018-10-28T02:30 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          )
        },
        test("Error accumulation") {
          case class Project(name: String, age: Int)
          implicit val decoder: JsonDecoder[Project] = DeriveJsonDecoder.gen
          val bad = """{"name":5,"age":"ZIO-JSON"}"""

          assert(bad.fromJsonValidation[Project])(
            isDoubleFailure(
              equalTo(""),
              equalTo("asd")
            )
          )
        } @@ ignore
      ),
      suite("fromJsonAST")(
        test("BigDecimal") {
          assert(Json.Num(123).as[BigDecimal])(isSucceeded(equalTo(BigDecimal(123))))
        },
        test("eithers") {
          val bernies =
            List(Json.Obj("a" -> Json.Num(1)), Json.Obj("left" -> Json.Num(1)), Json.Obj("Left" -> Json.Num(1)))
          val trumps =
            List(Json.Obj("b" -> Json.Num(2)), Json.Obj("right" -> Json.Num(2)), Json.Obj("Right" -> Json.Num(2)))

          assert(bernies.map(_.as[Either[Int, Int]]))(
            forall(isSucceeded(isLeft(equalTo(1))))
          ) && assert(trumps.map(_.as[Either[Int, Int]]))(
            forall(isSucceeded(isRight(equalTo(2))))
          )
        },
        test("parameterless products") {
          import exampleproducts._
          assert(Json.Obj().as[Parameterless])(isSucceeded(equalTo(Parameterless()))) &&
          assert(Json.Null.as[Parameterless])(isSucceeded(equalTo(Parameterless()))) &&
          assert(Json.Obj("field" -> Json.Str("value")).as[Parameterless])(isSucceeded(equalTo(Parameterless())))
        },
        test("no extra fields") {
          import exampleproducts._

          assert(Json.Obj("s" -> Json.Str("")).as[OnlyString])(isSucceeded(equalTo(OnlyString("")))) &&
          assert(Json.Obj("s" -> Json.Str(""), "t" -> Json.Str("")).as[OnlyString])(
            isSingleFailure(equalTo("(invalid extra field)"))
          )
        },
        test("preserve error path") {
          import exampleproducts._

          assert(Json.Obj("is" -> Json.Arr(Json.Obj("str" -> Json.Num(1)))).as[Outer])(
            isSingleFailure(equalTo(".is[0].str(Not a string value)"))
          )
        },
        test("default field value") {
          import exampleproducts._

          assert(Json.Obj().as[DefaultString])(isSucceeded(equalTo(DefaultString("")))) &&
          assert(Json.Obj("s" -> Json.Null).as[DefaultString])(isSucceeded(equalTo(DefaultString(""))))
        },
        test("aliases") {
          import exampleproducts._

          val expected = Aliases(a = 7, d = 15)
          assert(Json.Obj("a" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(isSucceeded(equalTo(expected))) &&
          assert(Json.Obj("b" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(isSucceeded(equalTo(expected))) &&
          assert(Json.Obj("c" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(isSucceeded(equalTo(expected))) &&
          assert(Json.Obj("a" -> Json.Num(7), "b" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(
            isSingleFailure(equalTo("(duplicate)"))
          ) &&
          assert(Json.Obj("b" -> Json.Num(7), "c" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(
            isSingleFailure(equalTo("(duplicate)"))
          )
        },
        test("sum encoding") {
          import examplesum._

          assert(Json.Obj("Child1" -> Json.Obj()).as[Parent])(isSucceeded(equalTo(Child1()))) &&
          assert(Json.Obj("Child2" -> Json.Obj()).as[Parent])(isSucceeded(equalTo(Child2()))) &&
          assert(Json.Obj("type" -> Json.Str("Child1")).as[Parent])(isSingleFailure(equalTo("(Invalid disambiguator)")))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert(Json.Obj("hint" -> Json.Str("Cain")).as[Parent])(isSucceeded(equalTo(Child1()))) &&
          assert(Json.Obj("hint" -> Json.Str("Abel")).as[Parent])(isSucceeded(equalTo(Child2()))) &&
          assert(Json.Obj("hint" -> Json.Str("Samson")).as[Parent])(
            isSingleFailure(equalTo("(Invalid disambiguator)"))
          ) &&
          assert(Json.Obj("Cain" -> Json.Obj()).as[Parent])(isSingleFailure(equalTo("(Missing hint 'hint')")))
        },
        test("Seq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Seq("5XL", "2XL", "XL")

          assert(json.as[Seq[String]])(isSucceeded(equalTo(expected)))
        },
        test("IndexedSeq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = IndexedSeq("5XL", "2XL", "XL")

          assert(json.as[IndexedSeq[String]])(isSucceeded(equalTo(expected)))
        },
        test("LinearSeq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.LinearSeq("5XL", "2XL", "XL")

          assert(json.as[immutable.LinearSeq[String]])(isSucceeded(equalTo(expected)))
        },
        test("ListSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.ListSet("5XL", "2XL", "XL")

          assert(json.as[immutable.ListSet[String]])(isSucceeded(equalTo(expected)))
        },
        test("TreeSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.TreeSet("5XL", "2XL", "XL")

          assert(json.as[immutable.TreeSet[String]])(isSucceeded(equalTo(expected)))
        },
        test("Vector") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Vector("5XL", "2XL", "XL")

          assert(json.as[Vector[String]])(isSucceeded(equalTo(expected)))
        },
        test("SortedSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(json.as[immutable.SortedSet[String]])(isSucceeded(equalTo(expected)))
        },
        test("HashSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(json.as[immutable.HashSet[String]])(isSucceeded(equalTo(expected)))
        },
        test("Set") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Set("5XL", "2XL", "XL")

          assert(json.as[Set[String]])(isSucceeded(equalTo(expected)))
        },
        test("Map") {
          val json     = Json.Obj("5XL" -> Json.Num(3), "2XL" -> Json.Num(14), "XL" -> Json.Num(159))
          val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(json.as[Map[String, Int]])(isSucceeded(equalTo(expected)))
        },
        test("SortedMap") {
          val json     = Json.Obj("5XL" -> Json.Num(3), "2XL" -> Json.Num(14), "XL" -> Json.Num(159))
          val expected = SortedMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(json.as[SortedMap[String, Int]])(isSucceeded(equalTo(expected)))
        },
        test("Map, custom keys") {
          val json     = Json.Obj("1" -> Json.Str("a"), "2" -> Json.Str("b"))
          val expected = Map(1 -> "a", 2 -> "b")

          assert(json.as[Map[Int, String]])(isSucceeded(equalTo(expected)))
        },
        test("zio.Chunk") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Chunk("5XL", "2XL", "XL")

          assert(json.as[Chunk[String]])(isSucceeded(equalTo(expected)))
        },
        test("zio.NonEmptyChunk") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assert(json.as[NonEmptyChunk[String]])(isSucceeded(equalTo(expected)))
        },
        test("java.util.UUID") {
          val ok1  = Json.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f8")
          val ok2  = Json.Str("0000000064D7C38D-FD-14-32-70AFE4B0f8")
          val ok3  = Json.Str("0-0-0-0-0")
          val bad1 = Json.Str("")
          val bad2 = Json.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f80")
          val bad3 = Json.Str("64d7c38d-2afd-4514-983-4e70afe4b0f80")
          val bad4 = Json.Str("64d7c38d-2afd--9832-4e70afe4b0f8")
          val bad5 = Json.Str("64d7c38d-2afd-XXXX-9832-4e70afe4b0f8")
          val bad6 = Json.Str("64d7c38d-2afd-X-9832-4e70afe4b0f8")
          val bad7 = Json.Str("0-0-0-0-00000000000000000")

          assert(ok1.as[UUID])(isSucceeded(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))) &&
          assert(ok2.as[UUID])(isSucceeded(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AFE4B0f8")))) &&
          assert(ok3.as[UUID])(isSucceeded(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))) &&
          assert(bad1.as[UUID])(isSingleFailure(containsString("Invalid UUID: "))) &&
          assert(bad2.as[UUID])(isSingleFailure(containsString("Invalid UUID: UUID string too large"))) &&
          assert(bad3.as[UUID])(
            isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd-4514-983-4e70afe4b0f80"))
          ) &&
          assert(bad4.as[UUID])(isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd--9832-4e70afe4b0f8"))) &&
          assert(bad5.as[UUID])(
            isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"))
          ) &&
          assert(bad6.as[UUID])(isSingleFailure(containsString("Invalid UUID: 64d7c38d-2afd-X-9832-4e70afe4b0f8"))) &&
          assert(bad7.as[UUID])(isSingleFailure(containsString("Invalid UUID: 0-0-0-0-00000000000000000")))
        }
      )
    )

  object exampleproducts {

    case class Parameterless()

    object Parameterless {

      implicit val decoder: JsonDecoder[Parameterless] =
        DeriveJsonDecoder.gen[Parameterless]
    }

    @jsonNoExtraFields
    case class OnlyString(s: String)

    object OnlyString {

      implicit val decoder: JsonDecoder[OnlyString] =
        DeriveJsonDecoder.gen[OnlyString]
    }

    case class DefaultString(s: String = "")

    object DefaultString {

      implicit val decoder: JsonDecoder[DefaultString] =
        DeriveJsonDecoder.gen[DefaultString]
    }

    case class Inner(str: String)

    object Inner {
      implicit val decoder: JsonDecoder[Inner] = DeriveJsonDecoder.gen
    }

    case class Outer(is: Chunk[Inner])

    object Outer {
      implicit val decoder: JsonDecoder[Outer] = DeriveJsonDecoder.gen
    }

    case class Aliases(@jsonAliases("b", "c") a: Int, d: Int)

    object Aliases {
      implicit val decoder: JsonDecoder[Aliases] = DeriveJsonDecoder.gen
    }

  }

  object examplesum {

    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    case class Child1() extends Parent

    case class Child2() extends Parent

  }

  object examplealtsum {

    @jsonDiscriminator("hint")
    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    @jsonHint("Cain")
    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent

  }

  object logEvent {

    case class Event(at: Long, message: String)

    implicit val eventDecoder: JsonDecoder[Event] = DeriveJsonDecoder.gen[Event]
    implicit val eventEncoder: JsonEncoder[Event] = DeriveJsonEncoder.gen[Event]
  }

}
