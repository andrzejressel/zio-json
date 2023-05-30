package zio.json.interop

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.refineV
import zio.json._
import zio.prelude.Validation

package object refined {
  implicit def encodeRefined[A: JsonEncoder, B]: JsonEncoder[A Refined B] =
    JsonEncoder[A].contramap(_.value)

  implicit def decodeRefined[A: JsonDecoder, P](implicit V: Validate[A, P]): JsonDecoder[A Refined P] =
    JsonDecoder[A].mapOrFail(data => Validation.fromEither(refineV[P](data)))

  implicit def encodeFieldRefined[A: JsonFieldEncoder, B]: JsonFieldEncoder[A Refined B] =
    JsonFieldEncoder[A].contramap(_.value)

  implicit def decodeFieldRefined[A: JsonFieldDecoder, P](implicit V: Validate[A, P]): JsonFieldDecoder[A Refined P] =
    JsonFieldDecoder[A].mapOrFail(refineV[P](_))
}
