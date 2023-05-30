package zio.json

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.circe
import org.openjdk.jmh.annotations._
import play.api.libs.{json => Play}
import testzio.json.TestUtils._
import testzio.json.data.twitter._
import zio.json.TwitterAPIBenchmarks._
import zio.prelude.Validation

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

// reference for the format of tweets: https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets.html

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class TwitterAPIBenchmarks {
  var jsonString, jsonStringCompact, jsonStringErr: String    = _
  var jsonChars, jsonCharsCompact, jsonCharsErr: CharSequence = _
  var decoded: List[Tweet]                                    = _

  @Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("twitter_api_response.json")
    jsonChars = asChars(jsonString)
    jsonStringCompact = getResourceAsString("twitter_api_compact_response.json")
    jsonCharsCompact = asChars(jsonStringCompact)
    jsonStringErr = getResourceAsString("twitter_api_error_response.json")
    jsonCharsErr = asChars(jsonStringErr)

    decoded = circe.parser.decode[List[Tweet]](jsonString).toOption.get

    assert(decodeJsoniterSuccess1() == decodeZioSuccess1())
    assert(decodeJsoniterSuccess2() == decodeZioSuccess1())
    assert(decodeJsoniterError().toEither.isLeft)

    assert(decodeCirceSuccess1() == decodeZioSuccess1())
    assert(decodeCirceSuccess2() == decodeZioSuccess2())
    assert(decodeCirceError().toEither.isLeft)

    assert(decodePlaySuccess1() == decodeZioSuccess1())
    assert(decodePlaySuccess2() == decodeZioSuccess2())
    assert(decodePlayError().toEither.isLeft)

    assert(decodeZioError().toEither.isLeft)
  }

  @Benchmark
  def decodeJsoniterSuccess1(): Validation[String, List[Tweet]] =
    Validation(readFromArray(jsonString.getBytes(UTF_8)))
      .mapError(_.toString)

  @Benchmark
  def decodeJsoniterSuccess2(): Validation[String, List[Tweet]] =
    Validation(readFromArray(jsonStringCompact.getBytes(UTF_8)))
      .mapError(_.toString)

  @Benchmark
  def decodeJsoniterError(): Validation[String, List[Tweet]] =
    Validation(readFromArray(jsonStringErr.getBytes(UTF_8)))
      .mapError(_.toString)

  @Benchmark
  def decodeCirceSuccess1(): Validation[circe.Error, List[Tweet]] =
    Validation.fromEither(circe.parser.decode[List[Tweet]](jsonString))

  @Benchmark
  def decodeCirceSuccess2(): Validation[circe.Error, List[Tweet]] =
    Validation.fromEither(circe.parser.decode[List[Tweet]](jsonStringCompact))

  @Benchmark
  def encodeCirce(): String = {
    import io.circe.syntax._

    decoded.asJson.noSpaces
  }

  @Benchmark
  def decodeCirceError(): Validation[circe.Error, List[Tweet]] =
    Validation.fromEither(circe.parser.decode[List[Tweet]](jsonStringErr))

  @Benchmark
  def decodePlaySuccess1(): Validation[String, List[Tweet]] =
    Validation(Play.Json.parse(jsonString).as[List[Tweet]])
      .mapError(_.toString)

  @Benchmark
  def decodePlaySuccess2(): Validation[String, List[Tweet]] =
    Validation(Play.Json.parse(jsonStringCompact).as[List[Tweet]])
      .mapError(_.toString)

  @Benchmark
  def encodePlay(): String =
    Play.Json.stringify(implicitly[Play.Writes[List[Tweet]]].writes(decoded))

  @Benchmark
  def decodePlayError(): Validation[String, List[Tweet]] =
    Validation(Play.Json.parse(jsonStringErr).as[List[Tweet]])
      .mapError(_.toString)

  @Benchmark
  def decodeZioSuccess1(): Validation[String, List[Tweet]] =
    jsonChars.fromJsonValidation[List[Tweet]]

  @Benchmark
  def decodeZioSuccess2(): Validation[String, List[Tweet]] =
    jsonCharsCompact.fromJsonValidation[List[Tweet]]

  @Benchmark
  def encodeZio(): CharSequence =
    JsonEncoder[List[Tweet]].encodeJson(decoded, None)

  @Benchmark
  def decodeZioError(): Validation[String, List[Tweet]] =
    jsonCharsErr.fromJsonValidation[List[Tweet]]

}

object TwitterAPIBenchmarks {
  // these avoid the List implicit from being recreated every time
  implicit val zListTweet: JsonDecoder[List[Tweet]] =
    JsonDecoder.list[Tweet]
  implicit val cListTweet: circe.Decoder[List[Tweet]] =
    circe.Decoder.decodeList[Tweet]
  implicit val codec: JsonValueCodec[List[Tweet]] =
    JsonCodecMaker.make
}
