package zio.json

import zio.NonEmptyChunk
import zio.prelude.Validation
import zio.prelude.ZValidation._
import zio.test.{Assertion, TestArrow, TestTrace}

object ValidationAssertions {
  def isSucceeded[A](assertion: Assertion[A]): Assertion[Validation[Any, A]] =
    Assertion[Validation[Any, A]](
      asSucceeded[A].withCode("isSucceeded") >>> assertion.arrow
    )

  def isSingleFailure[A](assertion: Assertion[A]): Assertion[Validation[A, Any]] =
    Assertion[Validation[A, Any]](
      asFailure[A].withCode("isFailure") >>> asSingle >>> assertion.arrow
    )

  def asSucceeded[A]: TestArrow[Validation[_, A], A] =
    TestArrow
      .make[Validation[_, A], A] {
        case Failure(_, _) => TestTrace.fail("Validation failed")
        case Success(_, value) => TestTrace.succeed(value)
      }


  def asFailure[A]: TestArrow[Validation[A, _], NonEmptyChunk[A]] =
    TestArrow
      .make[Validation[A, _], NonEmptyChunk[A]] {
        case Success(_, _) => TestTrace.fail("Validation succeeded")
        case Failure(_, value) => TestTrace.succeed(value)
      }


  def asSingle[A]: TestArrow[NonEmptyChunk[A], A] =
    TestArrow
      .make[NonEmptyChunk[A], A]{ chunk =>
        if (chunk.size != 1) {
          TestTrace.fail("Chunk has multiple elements")
        } else {
          TestTrace.succeed(chunk.head)
        }
      }

}
