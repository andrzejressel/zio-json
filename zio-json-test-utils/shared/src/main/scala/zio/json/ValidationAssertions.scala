package zio.json

import zio.NonEmptyChunk
import zio.prelude.Validation
import zio.prelude.ZValidation._
import zio.test.internal.SmartAssertions.hasAt
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

  def isDoubleFailure[A](assertion1: Assertion[A], assertion2: Assertion[A]): Assertion[Validation[A, Any]] =
    Assertion[Validation[A, Any]](
      asFailure[A].withCode("isFailure") >>> asDouble >>> hasAt(0) >>> assertion1.arrow
    ) && Assertion[Validation[A, Any]](
      asFailure[A].withCode("isFailure") >>> asDouble >>> hasAt(1) >>> assertion2.arrow
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
          TestTrace.fail(s"Invalid number of errors: ${chunk.size}")
        } else {
          TestTrace.succeed(chunk.head)
        }
      }

  def asDouble[A]: TestArrow[NonEmptyChunk[A], Seq[A]] =
    TestArrow
      .make[NonEmptyChunk[A], Seq[A]] { chunk =>
        if (chunk.size != 2) {
          TestTrace.fail(s"Invalid number of errors: ${chunk.size}")
        } else {
          TestTrace.succeed(Seq(chunk(0), chunk(1)))
        }
      }
}
