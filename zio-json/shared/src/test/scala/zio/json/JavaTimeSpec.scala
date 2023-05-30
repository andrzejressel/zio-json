package testzio.json

import zio.json.ValidationAssertions.{ isSingleFailure, isSucceeded }
import zio.json._
import zio.test.Assertion._
import zio.test._

import java.time._
import java.time.format.DateTimeFormatter

// zioJsonJVM/testOnly testzio.json.JavaTimeSpec
object JavaTimeSpec extends ZIOSpecDefault {

  private def stringify(s: Any): String = s""" "${s.toString}" """

  private def equalToStringified(expected: String) = equalTo(s""""$expected"""")

  val spec: Spec[Environment, Any] =
    suite("java.time")(
      suite("Encoder")(
        test("DayOfWeek") {
          assert(DayOfWeek.MONDAY.toJson)(equalToStringified("MONDAY")) &&
          assert(DayOfWeek.TUESDAY.toJson)(equalToStringified("TUESDAY")) &&
          assert(DayOfWeek.WEDNESDAY.toJson)(equalToStringified("WEDNESDAY")) &&
          assert(DayOfWeek.THURSDAY.toJson)(equalToStringified("THURSDAY")) &&
          assert(DayOfWeek.FRIDAY.toJson)(equalToStringified("FRIDAY")) &&
          assert(DayOfWeek.SATURDAY.toJson)(equalToStringified("SATURDAY")) &&
          assert(DayOfWeek.SUNDAY.toJson)(equalToStringified("SUNDAY"))
        },
        test("Duration") {
          assert(Duration.ofDays(0).toJson)(equalToStringified("PT0S")) &&
          assert(Duration.ofDays(1).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofHours(24).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofMinutes(1440).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofSeconds(Long.MaxValue, 999999999L).toJson)(
            equalToStringified("PT2562047788015215H30M7.999999999S")
          ) &&
          assert(""""PT-0.5S"""".fromJsonValidation[Duration].map(_.toString))(isSucceeded(equalTo("PT-0.5S"))) &&
          assert(""""-PT0.5S"""".fromJsonValidation[Duration].map(_.toString))(isSucceeded(equalTo("PT-0.5S")))
        },
        test("Instant") {
          val n = Instant.now()
          assert(Instant.EPOCH.toJson)(equalToStringified("1970-01-01T00:00:00Z")) &&
          assert(n.toJson)(equalToStringified(n.toString))
        },
        test("LocalDate") {
          val n = LocalDate.now()
          val p = LocalDate.of(2020, 1, 1)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_DATE))) &&
          assert(p.toJson)(equalToStringified("2020-01-01"))
        },
        test("LocalDateTime") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))) &&
          assert(p.toJson)(equalToStringified("2020-01-01T12:36:00"))
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_TIME))) &&
          assert(p.toJson)(equalToStringified("12:36:00"))
        },
        test("Month") {
          assert(Month.JANUARY.toJson)(equalToStringified("JANUARY")) &&
          assert(Month.FEBRUARY.toJson)(equalToStringified("FEBRUARY")) &&
          assert(Month.MARCH.toJson)(equalToStringified("MARCH")) &&
          assert(Month.APRIL.toJson)(equalToStringified("APRIL")) &&
          assert(Month.MAY.toJson)(equalToStringified("MAY")) &&
          assert(Month.JUNE.toJson)(equalToStringified("JUNE")) &&
          assert(Month.JULY.toJson)(equalToStringified("JULY")) &&
          assert(Month.AUGUST.toJson)(equalToStringified("AUGUST")) &&
          assert(Month.SEPTEMBER.toJson)(equalToStringified("SEPTEMBER")) &&
          assert(Month.OCTOBER.toJson)(equalToStringified("OCTOBER")) &&
          assert(Month.NOVEMBER.toJson)(equalToStringified("NOVEMBER")) &&
          assert(Month.DECEMBER.toJson)(equalToStringified("DECEMBER"))
        },
        test("MonthDay") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(p.toJson)(equalToStringified("--01-01"))
        },
        test("OffsetDateTime") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))) &&
          assert(p.toJson)(equalToStringified("2020-01-01T12:36:12Z"))
        },
        test("OffsetTime") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_OFFSET_TIME))) &&
          assert(p.toJson)(equalToStringified("12:36:12-04:00"))
        },
        test("Period") {
          assert(Period.ZERO.toJson)(equalToStringified("P0D")) &&
          assert(Period.ofDays(1).toJson)(equalToStringified("P1D")) &&
          assert(Period.ofMonths(2).toJson)(equalToStringified("P2M")) &&
          assert(Period.ofWeeks(52).toJson)(equalToStringified("P364D")) &&
          assert(Period.ofYears(10).toJson)(equalToStringified("P10Y"))
        },
        test("Year") {
          val n = Year.now()
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(Year.of(1999).toJson)(equalToStringified("1999")) &&
          assert(Year.of(10000).toJson)(equalToStringified("+10000"))
        },
        test("YearMonth") {
          val n = YearMonth.now()
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(YearMonth.of(1999, 12).toJson)(equalToStringified("1999-12")) &&
          assert(YearMonth.of(1999, 1).toJson)(equalToStringified("1999-01"))
        },
        test("ZonedDateTime") {
          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))) &&
          assert(est.toJson)(equalToStringified("2020-01-01T12:36:00-05:00[America/New_York]")) &&
          assert(utc.toJson)(equalToStringified("2020-01-01T12:36:00Z[Etc/UTC]"))
        },
        test("ZoneId") {
          assert(ZoneId.of("America/New_York").toJson)(equalToStringified("America/New_York")) &&
          assert(ZoneId.of("Etc/UTC").toJson)(equalToStringified("Etc/UTC")) &&
          assert(ZoneId.of("Pacific/Auckland").toJson)(equalToStringified("Pacific/Auckland")) &&
          assert(ZoneId.of("Asia/Shanghai").toJson)(equalToStringified("Asia/Shanghai")) &&
          assert(ZoneId.of("Africa/Cairo").toJson)(equalToStringified("Africa/Cairo"))
        },
        test("ZoneOffset") {
          assert(ZoneOffset.UTC.toJson)(equalToStringified("Z")) &&
          assert(ZoneOffset.ofHours(5).toJson)(equalToStringified("+05:00")) &&
          assert(ZoneOffset.ofHours(-5).toJson)(equalToStringified("-05:00"))
        }
      ),
      suite("Decoder")(
        test("DayOfWeek") {
          assert(stringify("MONDAY").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("TUESDAY").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.TUESDAY))) &&
          assert(stringify("WEDNESDAY").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.WEDNESDAY))) &&
          assert(stringify("THURSDAY").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.THURSDAY))) &&
          assert(stringify("FRIDAY").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.FRIDAY))) &&
          assert(stringify("SATURDAY").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.SATURDAY))) &&
          assert(stringify("SUNDAY").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.SUNDAY))) &&
          assert(stringify("monday").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("MonDay").fromJsonValidation[DayOfWeek])(isSucceeded(equalTo(DayOfWeek.MONDAY)))
        },
        test("Duration") {
          assert(stringify("PT24H").fromJsonValidation[Duration])(isSucceeded(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("-PT24H").fromJsonValidation[Duration])(isSucceeded(equalTo(Duration.ofHours(-24)))) &&
          assert(stringify("P1D").fromJsonValidation[Duration])(isSucceeded(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("P1DT0H").fromJsonValidation[Duration])(isSucceeded(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("PT2562047788015215H30M7.999999999S").fromJsonValidation[Duration])(
            isSucceeded(equalTo(Duration.ofSeconds(Long.MaxValue, 999999999L)))
          )
        },
        test("Instant") {
          val n = OffsetDateTime.now()
          val p = n.toInstant
          assert(stringify("1970-01-01T00:00:00Z").fromJsonValidation[Instant])(isSucceeded(equalTo(Instant.EPOCH))) &&
          assert(stringify("1970-01-01T00:00:00.Z").fromJsonValidation[Instant])(isSucceeded(equalTo(Instant.EPOCH))) &&
          assert(stringify(p).fromJsonValidation[Instant])(isSucceeded(equalTo(p))) &&
          assert(stringify(n).fromJsonValidation[Instant])(isSucceeded(equalTo(p)))
        },
        test("LocalDate") {
          val n = LocalDate.now()
          val p = LocalDate.of(2000, 2, 29)
          assert(stringify(n).fromJsonValidation[LocalDate])(isSucceeded(equalTo(n))) &&
          assert(stringify(p).fromJsonValidation[LocalDate])(isSucceeded(equalTo(p)))
        },
        test("LocalDateTime") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(stringify(n).fromJsonValidation[LocalDateTime])(isSucceeded(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36").fromJsonValidation[LocalDateTime])(isSucceeded(equalTo(p))) &&
          assert(stringify("2020-01-01T12:36:00.").fromJsonValidation[LocalDateTime])(isSucceeded(equalTo(p)))
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(stringify(n).fromJsonValidation[LocalTime])(isSucceeded(equalTo(n))) &&
          assert(stringify("12:36").fromJsonValidation[LocalTime])(isSucceeded(equalTo(p))) &&
          assert(stringify("12:36:00.").fromJsonValidation[LocalTime])(isSucceeded(equalTo(p)))
        },
        test("Month") {
          assert(stringify("JANUARY").fromJsonValidation[Month])(isSucceeded(equalTo(Month.JANUARY))) &&
          assert(stringify("FEBRUARY").fromJsonValidation[Month])(isSucceeded(equalTo(Month.FEBRUARY))) &&
          assert(stringify("MARCH").fromJsonValidation[Month])(isSucceeded(equalTo(Month.MARCH))) &&
          assert(stringify("APRIL").fromJsonValidation[Month])(isSucceeded(equalTo(Month.APRIL))) &&
          assert(stringify("MAY").fromJsonValidation[Month])(isSucceeded(equalTo(Month.MAY))) &&
          assert(stringify("JUNE").fromJsonValidation[Month])(isSucceeded(equalTo(Month.JUNE))) &&
          assert(stringify("JULY").fromJsonValidation[Month])(isSucceeded(equalTo(Month.JULY))) &&
          assert(stringify("AUGUST").fromJsonValidation[Month])(isSucceeded(equalTo(Month.AUGUST))) &&
          assert(stringify("SEPTEMBER").fromJsonValidation[Month])(isSucceeded(equalTo(Month.SEPTEMBER))) &&
          assert(stringify("OCTOBER").fromJsonValidation[Month])(isSucceeded(equalTo(Month.OCTOBER))) &&
          assert(stringify("NOVEMBER").fromJsonValidation[Month])(isSucceeded(equalTo(Month.NOVEMBER))) &&
          assert(stringify("DECEMBER").fromJsonValidation[Month])(isSucceeded(equalTo(Month.DECEMBER))) &&
          assert(stringify("december").fromJsonValidation[Month])(isSucceeded(equalTo(Month.DECEMBER))) &&
          assert(stringify("December").fromJsonValidation[Month])(isSucceeded(equalTo(Month.DECEMBER)))
        },
        test("MonthDay") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)
          assert(stringify(n).fromJsonValidation[MonthDay])(isSucceeded(equalTo(n))) &&
          assert(stringify("--01-01").fromJsonValidation[MonthDay])(isSucceeded(equalTo(p)))
        },
        test("OffsetDateTime") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)
          assert(stringify(n).fromJsonValidation[OffsetDateTime])(isSucceeded(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36:12Z").fromJsonValidation[OffsetDateTime])(isSucceeded(equalTo(p))) &&
          assert(stringify("2020-01-01T12:36:12.Z").fromJsonValidation[OffsetDateTime])(isSucceeded(equalTo(p)))
        },
        test("OffsetTime") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(stringify(n).fromJsonValidation[OffsetTime])(isSucceeded(equalTo(n))) &&
          assert(stringify("12:36:12-04:00").fromJsonValidation[OffsetTime])(isSucceeded(equalTo(p))) &&
          assert(stringify("12:36:12.-04:00").fromJsonValidation[OffsetTime])(isSucceeded(equalTo(p)))
        },
        test("Period") {
          assert(stringify("P0D").fromJsonValidation[Period])(isSucceeded(equalTo(Period.ZERO))) &&
          assert(stringify("P1D").fromJsonValidation[Period])(isSucceeded(equalTo(Period.ofDays(1)))) &&
          assert(stringify("P-1D").fromJsonValidation[Period])(isSucceeded(equalTo(Period.ofDays(-1)))) &&
          assert(stringify("-P1D").fromJsonValidation[Period])(isSucceeded(equalTo(Period.ofDays(-1)))) &&
          assert(stringify("P2M").fromJsonValidation[Period])(isSucceeded(equalTo(Period.ofMonths(2)))) &&
          assert(stringify("P364D").fromJsonValidation[Period])(isSucceeded(equalTo(Period.ofWeeks(52)))) &&
          assert(stringify("P10Y").fromJsonValidation[Period])(isSucceeded(equalTo(Period.ofYears(10))))
        },
        test("Year") {
          val n = Year.now()
          assert(stringify(n).fromJsonValidation[Year])(isSucceeded(equalTo(n))) &&
          assert(stringify("1999").fromJsonValidation[Year])(isSucceeded(equalTo(Year.of(1999)))) &&
          assert(stringify("+10000").fromJsonValidation[Year])(isSucceeded(equalTo(Year.of(10000))))
        },
        test("YearMonth") {
          val n = YearMonth.now()
          assert(stringify(n).fromJsonValidation[YearMonth])(isSucceeded(equalTo(n))) &&
          assert(stringify("1999-12").fromJsonValidation[YearMonth])(isSucceeded(equalTo(YearMonth.of(1999, 12)))) &&
          assert(stringify("1999-01").fromJsonValidation[YearMonth])(isSucceeded(equalTo(YearMonth.of(1999, 1))))
        },
        test("ZonedDateTime") {
          def zdtAssert(actual: String, expected: ZonedDateTime): TestResult =
            assert(stringify(actual).fromJsonValidation[ZonedDateTime].map(_.toString))(isSucceeded(equalTo(expected.toString)))

          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          val gmt = ZonedDateTime.of(ld, ZoneId.of("+00:00"))

          zdtAssert(n.toString, n) &&
          zdtAssert("2020-01-01T12:36:00-05:00[America/New_York]", est) &&
          zdtAssert("2020-01-01T12:36:00Z[Etc/UTC]", utc) &&
          zdtAssert("2020-01-01T12:36:00.Z[Etc/UTC]", utc) &&
          zdtAssert("2020-01-01T12:36:00+00:00[+00:00]", gmt) &&
          zdtAssert(
            "2018-02-01T00:00Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 2, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "2018-03-01T00:00:00Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 3, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "2018-04-01T00:00:00.000Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 4, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "+999999999-12-31T23:59:59.999999999+18:00",
            ZonedDateTime.of(LocalDateTime.MAX, ZoneOffset.MAX)
          ) &&
          zdtAssert(
            "+999999999-12-31T23:59:59.999999999-18:00",
            ZonedDateTime.of(LocalDateTime.MAX, ZoneOffset.MIN)
          ) &&
          zdtAssert("-999999999-01-01T00:00:00+18:00", ZonedDateTime.of(LocalDateTime.MIN, ZoneOffset.MAX)) &&
          zdtAssert("-999999999-01-01T00:00:00-18:00", ZonedDateTime.of(LocalDateTime.MIN, ZoneOffset.MIN)) &&
          zdtAssert(
            "2012-10-28T02:00:00+01:00[Europe/Berlin]",
            OffsetDateTime.parse("2012-10-28T02:00:00+01:00").atZoneSameInstant(ZoneId.of("Europe/Berlin"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+01:00[Europe/Warsaw]",
            ZonedDateTime.parse("2018-03-25T02:30+01:00[Europe/Warsaw]")
          ) &&
          zdtAssert(
            "2018-03-25T02:30+00:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+00:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+02:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+02:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+03:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+03:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+00:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+00:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+01:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+01:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+02:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+02:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+03:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+03:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          )
        },
        test("ZoneId") {
          assert(stringify("America/New_York").fromJsonValidation[ZoneId])(
            isSucceeded(
              equalTo(
                ZoneId.of("America/New_York")
              )
            )
          ) &&
          assert(stringify("Etc/UTC").fromJsonValidation[ZoneId])(isSucceeded(equalTo(ZoneId.of("Etc/UTC")))) &&
          assert(stringify("Pacific/Auckland").fromJsonValidation[ZoneId])(
            isSucceeded(
              equalTo(
                ZoneId.of("Pacific/Auckland")
              )
            )
          ) &&
          assert(stringify("Asia/Shanghai").fromJsonValidation[ZoneId])(
            isSucceeded(equalTo(ZoneId.of("Asia/Shanghai")))
          ) &&
          assert(stringify("Africa/Cairo").fromJsonValidation[ZoneId])(isSucceeded(equalTo(ZoneId.of("Africa/Cairo"))))
        },
        test("ZoneOffset") {
          assert(stringify("Z").fromJsonValidation[ZoneOffset])(isSucceeded(equalTo(ZoneOffset.UTC))) &&
          assert(stringify("+05:00").fromJsonValidation[ZoneOffset])(isSucceeded(equalTo(ZoneOffset.ofHours(5)))) &&
          assert(stringify("-05:00").fromJsonValidation[ZoneOffset])(isSucceeded(equalTo(ZoneOffset.ofHours(-5))))
        }
      ),
      suite("Decoder Sad Path")(
        test("DayOfWeek") {
          assert(stringify("foody").fromJsonValidation[DayOfWeek])(
            isSingleFailure(
              equalTo("(No enum constant java.time.DayOfWeek.FOODY)") || // JVM
                equalTo("(Unrecognized day of week name: FOODY)")
            ) // Scala.js
          )
        },
        test("Duration") {
          assert("""""""".fromJsonValidation[Duration])(
            isSingleFailure(containsString(" is not a valid ISO-8601 format, illegal duration at index 0"))
          ) &&
          assert(""""X"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("X is not a valid ISO-8601 format, expected 'P' or '-' at index 0"))
          ) &&
          assert(""""P"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P is not a valid ISO-8601 format, illegal duration at index 1"))
          ) &&
          assert(""""-"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("- is not a valid ISO-8601 format, illegal duration at index 1"))
          ) &&
          assert(""""-X"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("-X is not a valid ISO-8601 format, expected 'P' at index 1"))
          ) &&
          assert(""""PXD"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("PXD is not a valid ISO-8601 format, expected '-' or digit at index 1"))
          ) &&
          assert(""""P-"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P- is not a valid ISO-8601 format, illegal duration at index 2"))
          ) &&
          assert(""""P-XD"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P-XD is not a valid ISO-8601 format, expected digit at index 2"))
          ) &&
          assert(""""P1XD"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P1XD is not a valid ISO-8601 format, expected 'D' or digit at index 2"))
          ) &&
          assert(""""PT"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("PT is not a valid ISO-8601 format, illegal duration at index 2"))
          ) &&
          assert(""""PT0SX"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("PT0SX is not a valid ISO-8601 format, illegal duration at index 4"))
          ) &&
          assert(""""P1DT"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P1DT is not a valid ISO-8601 format, illegal duration at index 4"))
          ) &&
          assert(""""P106751991167301D"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P106751991167301D is not a valid ISO-8601 format, illegal duration at index 16")
            )
          ) &&
          assert(""""P1067519911673000D"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P1067519911673000D is not a valid ISO-8601 format, illegal duration at index 17")
            )
          ) &&
          assert(""""P-106751991167301D"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P-106751991167301D is not a valid ISO-8601 format, illegal duration at index 17")
            )
          ) &&
          assert(""""P1DX1H"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P1DX1H is not a valid ISO-8601 format, expected 'T' or '\"' at index 3"))
          ) &&
          assert(""""P1DTXH"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P1DTXH is not a valid ISO-8601 format, expected '-' or digit at index 4"))
          ) &&
          assert(""""P1DT-XH"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P1DT-XH is not a valid ISO-8601 format, expected digit at index 5"))
          ) &&
          assert(""""P1DT1XH"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString(
                "P1DT1XH is not a valid ISO-8601 format, expected 'H' or 'M' or 'S or '.' or digit at index 5"
              )
            )
          ) &&
          assert(""""P1DT1H1XM"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P1DT1H1XM is not a valid ISO-8601 format, expected 'M' or 'S or '.' or digit at index 7")
            )
          ) &&
          assert(""""P0DT2562047788015216H"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT2562047788015216H is not a valid ISO-8601 format, illegal duration at index 20")
            )
          ) &&
          assert(""""P0DT-2562047788015216H"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT-2562047788015216H is not a valid ISO-8601 format, illegal duration at index 21")
            )
          ) &&
          assert(""""P0DT153722867280912931M"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT153722867280912931M is not a valid ISO-8601 format, illegal duration at index 22")
            )
          ) &&
          assert(""""P0DT-153722867280912931M"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT-153722867280912931M is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT9223372036854775808S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT9223372036854775808S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT92233720368547758000S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT92233720368547758000S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT-9223372036854775809S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT-9223372036854775809S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P1DT1H1MXS"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P1DT1H1MXS is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 8")
            )
          ) &&
          assert(""""P1DT1H1M-XS"""".fromJsonValidation[Duration])(
            isSingleFailure(containsString("P1DT1H1M-XS is not a valid ISO-8601 format, expected digit at index 9"))
          ) &&
          assert(""""P1DT1H1M0XS"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P1DT1H1M0XS is not a valid ISO-8601 format, expected 'S or '.' or digit at index 9")
            )
          ) &&
          assert(""""P1DT1H1M0.XS"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P1DT1H1M0.XS is not a valid ISO-8601 format, expected 'S' or digit at index 10")
            )
          ) &&
          assert(""""P1DT1H1M0.012345678XS"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P1DT1H1M0.012345678XS is not a valid ISO-8601 format, expected 'S' at index 19")
            )
          ) &&
          assert(""""P1DT1H1M0.0123456789S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P1DT1H1M0.0123456789S is not a valid ISO-8601 format, expected 'S' at index 19")
            )
          ) &&
          assert(""""P0DT0H0M9223372036854775808S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString(
                "P0DT0H0M9223372036854775808S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P0DT0H0M92233720368547758080S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString(
                "P0DT0H0M92233720368547758080S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P0DT0H0M-9223372036854775809S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString(
                "P0DT0H0M-9223372036854775809S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P106751991167300DT24H"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P106751991167300DT24H is not a valid ISO-8601 format, illegal duration at index 20")
            )
          ) &&
          assert(""""P0DT2562047788015215H60M"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString("P0DT2562047788015215H60M is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT0H153722867280912930M60S"""".fromJsonValidation[Duration])(
            isSingleFailure(
              containsString(
                "P0DT0H153722867280912930M60S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          )
        },
        test("Instant") {
          assert(stringify("").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal instant at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020 is not a valid ISO-8601 format, illegal instant at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal instant at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal instant at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal instant at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal instant at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal instant at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, illegal instant at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal instant at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, illegal instant at index 23)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal instant at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, illegal instant at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal instant at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000001-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+1000000001-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("+3333333333-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+3333333333-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-1000000001-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(-1000000001-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal instant at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJsonValidation[Instant])(
            isSingleFailure(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalDate") {
          assert(stringify("").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal local date at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020 is not a valid ISO-8601 format, illegal local date at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal local date at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal local date at index 8)")
            )
          ) &&
          assert(stringify("2020-01-012").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-01-012 is not a valid ISO-8601 format, illegal local date at index 10)")
            )
          ) &&
          assert(stringify("X020-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(X020-01-01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2X20-01-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(20X0-01-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(202X-01-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020X01-01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-X1-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-0X-01 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-01X01 is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-01-X1 is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0X").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-01-0X is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("+X0000-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+X0000-01-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+1X000-01-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+10X00-01-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+100X0-01-01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+1000X-01-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+10000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+100000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+1000000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+1000000000-01-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(-1000000000-01-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(-0000-01-01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal local date at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-00-01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-13-01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-01-00 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-01-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-02-30 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-03-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-04-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-05-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-06-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-07-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-08-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-09-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-10-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-11-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32").fromJsonValidation[LocalDate])(
            isSingleFailure(
              equalTo("(2020-12-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalDateTime") {
          assert(stringify("").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal local date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020 is not a valid ISO-8601 format, illegal local date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal local date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal local date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal local date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal local date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(X020-01-01T01:01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2X20-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(20X0-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(202X-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020X01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-X1-01T01:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-0X-01T01:01 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01X01T01:01 is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-X1T01:01 is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-0XT01:01 is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01X01:01 is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' at index 16)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal local date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:X1 is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0X").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0X is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:60 is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' at index 19)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01.X is not a valid ISO-8601 format, illegal local date time at index 20)")
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+X0000-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+1X000-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+10X00-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+100X0-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+1000X-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+10000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+100000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+1000000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+1000000000-01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(-1000000000-01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(-0000-01-01T01:01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal local date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-00-01T01:01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-13-01T01:01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-00T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-01-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-02-30T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-03-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-04-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-05-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-06-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-07-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-08-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-09-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-10-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-11-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01").fromJsonValidation[LocalDateTime])(
            isSingleFailure(
              equalTo("(2020-12-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalTime") {
          assert(stringify("").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal local time at index 0)")
            )
          ) &&
          assert(stringify("0").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(0 is not a valid ISO-8601 format, illegal local time at index 0)")
            )
          ) &&
          assert(stringify("01:0").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:0 is not a valid ISO-8601 format, illegal local time at index 3)")
            )
          ) &&
          assert(stringify("X1:01").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(X1:01 is not a valid ISO-8601 format, expected digit at index 0)")
            )
          ) &&
          assert(stringify("0X:01").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(0X:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("24:01").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(24:01 is not a valid ISO-8601 format, illegal hour at index 1)")
            )
          ) &&
          assert(stringify("01X01").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01X01 is not a valid ISO-8601 format, expected ':' at index 2)")
            )
          ) &&
          assert(stringify("01:X1").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:X1 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("01:0X").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:0X is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("01:60").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:60 is not a valid ISO-8601 format, illegal minute at index 4)")
            )
          ) &&
          assert(stringify("01:01X").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:01X is not a valid ISO-8601 format, expected ':' at index 5)")
            )
          ) &&
          assert(stringify("01:01:0").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:01:0 is not a valid ISO-8601 format, illegal local time at index 6)")
            )
          ) &&
          assert(stringify("01:01:X1").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:01:X1 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("01:01:0X").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:01:0X is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("01:01:60").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:01:60 is not a valid ISO-8601 format, illegal second at index 7)")
            )
          ) &&
          assert(stringify("01:01:012").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:01:012 is not a valid ISO-8601 format, expected '.' at index 8)")
            )
          ) &&
          assert(stringify("01:01:01.X").fromJsonValidation[LocalTime])(
            isSingleFailure(
              equalTo("(01:01:01.X is not a valid ISO-8601 format, illegal local time at index 9)")
            )
          )
        },
        test("Month") {
          assert(stringify("FebTober").fromJsonValidation[Month])(
            isSingleFailure(
              equalTo("(No enum constant java.time.Month.FEBTOBER)") || // JVM
                equalTo("(Unrecognized month name: FEBTOBER)")
            ) // Scala.js
          )
        },
        test("MonthDay") {
          assert(stringify("").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("( is not a valid ISO-8601 format, illegal month day at index 0)"))
          ) &&
          assert(stringify("X-01-01").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(X-01-01 is not a valid ISO-8601 format, expected '-' at index 0)"))
          ) &&
          assert(stringify("-X01-01").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(-X01-01 is not a valid ISO-8601 format, expected '-' at index 1)"))
          ) &&
          assert(stringify("--X1-01").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--X1-01 is not a valid ISO-8601 format, expected digit at index 2)"))
          ) &&
          assert(stringify("--0X-01").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--0X-01 is not a valid ISO-8601 format, expected digit at index 3)"))
          ) &&
          assert(stringify("--00-01").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--00-01 is not a valid ISO-8601 format, illegal month at index 3)"))
          ) &&
          assert(stringify("--13-01").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--13-01 is not a valid ISO-8601 format, illegal month at index 3)"))
          ) &&
          assert(stringify("--01X01").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--01X01 is not a valid ISO-8601 format, expected '-' at index 4)"))
          ) &&
          assert(stringify("--01-X1").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--01-X1 is not a valid ISO-8601 format, expected digit at index 5)"))
          ) &&
          assert(stringify("--01-0X").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--01-0X is not a valid ISO-8601 format, expected digit at index 6)"))
          ) &&
          assert(stringify("--01-00").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--01-00 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--01-32").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--01-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--02-30").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--02-30 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--03-32").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--03-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--04-31").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--04-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--05-32").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--05-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--06-31").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--06-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--07-32").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--07-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--08-32").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--08-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--09-31").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--09-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--10-32").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--10-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--11-31").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--11-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--12-32").fromJsonValidation[MonthDay])(
            isSingleFailure(equalTo("(--12-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          )
        },
        test("OffsetDateTime") {
          assert(stringify("").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal offset date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020 is not a valid ISO-8601 format, illegal offset date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal offset date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal offset date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal offset date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal offset date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal offset date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, illegal offset date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal offset date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, illegal offset date time at index 23)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal offset date time at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, illegal offset date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal offset date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(-1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal offset date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJsonValidation[OffsetDateTime])(
            isSingleFailure(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("OffsetTime") {
          assert(stringify("").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal offset time at index 0)")
            )
          ) &&
          assert(stringify("0").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(0 is not a valid ISO-8601 format, illegal offset time at index 0)")
            )
          ) &&
          assert(stringify("01:0").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:0 is not a valid ISO-8601 format, illegal offset time at index 3)")
            )
          ) &&
          assert(stringify("X1:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(X1:01 is not a valid ISO-8601 format, expected digit at index 0)")
            )
          ) &&
          assert(stringify("0X:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(0X:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("24:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(24:01 is not a valid ISO-8601 format, illegal hour at index 1)")
            )
          ) &&
          assert(stringify("01X01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01X01 is not a valid ISO-8601 format, expected ':' at index 2)")
            )
          ) &&
          assert(stringify("01:X1").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:X1 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("01:0X").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:0X is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("01:60").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:60 is not a valid ISO-8601 format, illegal minute at index 4)")
            )
          ) &&
          assert(stringify("01:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo(
                "(01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 5)"
              )
            )
          ) &&
          assert(stringify("01:01X").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo(
                "(01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 5)"
              )
            )
          ) &&
          assert(stringify("01:01:0").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:0 is not a valid ISO-8601 format, illegal offset time at index 6)")
            )
          ) &&
          assert(stringify("01:01:X1Z").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:X1Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("01:01:0XZ").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:0XZ is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("01:01:60Z").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:60Z is not a valid ISO-8601 format, illegal second at index 7)")
            )
          ) &&
          assert(stringify("01:01:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo(
                "(01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 8)"
              )
            )
          ) &&
          assert(stringify("01:01:012").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo(
                "(01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 8)"
              )
            )
          ) &&
          assert(stringify("01:01:01.").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo(
                "(01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 9)"
              )
            )
          ) &&
          assert(stringify("01:01:01.X").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo(
                "(01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 9)"
              )
            )
          ) &&
          assert(stringify("01:01:01.123456789X").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo(
                "(01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 18)"
              )
            )
          ) &&
          assert(stringify("01:01:01ZX").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01ZX is not a valid ISO-8601 format, illegal offset time at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+X1:01:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+0").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+0 is not a valid ISO-8601 format, illegal offset time at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+0X:01:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 10)")
            )
          ) &&
          assert(stringify("01:01:01+19:01:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 10)")
            )
          ) &&
          assert(stringify("01:01:01+01X01:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01X01:01 is not a valid ISO-8601 format, illegal offset time at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:0").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:0 is not a valid ISO-8601 format, illegal offset time at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:X1:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:0X:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 13)")
            )
          ) &&
          assert(stringify("01:01:01+01:60:01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 13)")
            )
          ) &&
          assert(stringify("01:01:01+01:01X01").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:01X01 is not a valid ISO-8601 format, illegal offset time at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:0").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:01:0 is not a valid ISO-8601 format, illegal offset time at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:X1").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:0X").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 16)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:60").fromJsonValidation[OffsetTime])(
            isSingleFailure(
              equalTo("(01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 16)")
            )
          )
        },
        test("Period") {
          assert(stringify("").fromJsonValidation[Period])(
            isSingleFailure(equalTo("( is not a valid ISO-8601 format, illegal period at index 0)"))
          ) &&
          assert(stringify("X").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(X is not a valid ISO-8601 format, expected 'P' or '-' at index 0)"))
          ) &&
          assert(stringify("P").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P is not a valid ISO-8601 format, illegal period at index 1)"))
          ) &&
          assert(stringify("-").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(- is not a valid ISO-8601 format, illegal period at index 1)"))
          ) &&
          assert(stringify("PXY").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(PXY is not a valid ISO-8601 format, expected '-' or digit at index 1)"))
          ) &&
          assert(stringify("P-").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P- is not a valid ISO-8601 format, illegal period at index 2)"))
          ) &&
          assert(stringify("P-XY").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P-XY is not a valid ISO-8601 format, expected digit at index 2)"))
          ) &&
          assert(stringify("P1XY").fromJsonValidation[Period])(
            isSingleFailure(
              equalTo("(P1XY is not a valid ISO-8601 format, expected 'Y' or 'M' or 'W' or 'D' or digit at index 2)")
            )
          ) &&
          assert(stringify("P2147483648Y").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P2147483648Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470Y").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P21474836470Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649Y").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P-2147483649Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648M").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P2147483648M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470M").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P21474836470M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649M").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P-2147483649M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P2147483648W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P21474836470W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P-2147483649W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P2147483648D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P21474836470D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P-2147483649D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P1YXM").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1YXM is not a valid ISO-8601 format, expected '-' or digit at index 3)"))
          ) &&
          assert(stringify("P1Y-XM").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y-XM is not a valid ISO-8601 format, expected digit at index 4)"))
          ) &&
          assert(stringify("P1Y1XM").fromJsonValidation[Period])(
            isSingleFailure(
              equalTo("(P1Y1XM is not a valid ISO-8601 format, expected 'M' or 'W' or 'D' or digit at index 4)")
            )
          ) &&
          assert(stringify("P1Y2147483648M").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y2147483648M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470M").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y21474836470M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649M").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y-2147483649M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y2147483648W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y2147483648W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y21474836470W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y-2147483649W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y2147483648D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y2147483648D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y21474836470D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y-2147483649D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y1MXW").fromJsonValidation[Period])(
            isSingleFailure(
              equalTo("(P1Y1MXW is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 5)")
            )
          ) &&
          assert(stringify("P1Y1M-XW").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M-XW is not a valid ISO-8601 format, expected digit at index 6)"))
          ) &&
          assert(stringify("P1Y1M1XW").fromJsonValidation[Period])(
            isSingleFailure(
              equalTo("(P1Y1M1XW is not a valid ISO-8601 format, expected 'W' or 'D' or digit at index 6)")
            )
          ) &&
          assert(stringify("P1Y1M306783379W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M306783379W is not a valid ISO-8601 format, illegal period at index 14)"))
          ) &&
          assert(stringify("P1Y1M3067833790W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M3067833790W is not a valid ISO-8601 format, illegal period at index 14)"))
          ) &&
          assert(stringify("P1Y1M-306783379W").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M-306783379W is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M2147483648D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M2147483648D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M21474836470D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M21474836470D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M-2147483649D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M-2147483649D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M1WXD").fromJsonValidation[Period])(
            isSingleFailure(
              equalTo("(P1Y1M1WXD is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("P1Y1M1W-XD").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M1W-XD is not a valid ISO-8601 format, expected digit at index 8)"))
          ) &&
          assert(stringify("P1Y1M1W1XD").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M1W1XD is not a valid ISO-8601 format, expected 'D' or digit at index 8)"))
          ) &&
          assert(stringify("P1Y1M306783378W8D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M306783378W8D is not a valid ISO-8601 format, illegal period at index 16)"))
          ) &&
          assert(stringify("P1Y1M-306783378W-8D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M-306783378W-8D is not a valid ISO-8601 format, illegal period at index 18)"))
          ) &&
          assert(stringify("P1Y1M1W2147483647D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M1W2147483647D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M-1W-2147483648D").fromJsonValidation[Period])(
            isSingleFailure(
              equalTo("(P1Y1M-1W-2147483648D is not a valid ISO-8601 format, illegal period at index 19)")
            )
          ) &&
          assert(stringify("P1Y1M0W2147483648D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M0W2147483648D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M0W21474836470D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M0W21474836470D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M0W-2147483649D").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M0W-2147483649D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M1W1DX").fromJsonValidation[Period])(
            isSingleFailure(equalTo("(P1Y1M1W1DX is not a valid ISO-8601 format, illegal period at index 9)"))
          )
        },
        test("Year") {
          assert(stringify("").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("2").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(2 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("22").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(22 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("222").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(222 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("X020").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(X020 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(2X20 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(20X0 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(202X is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+X0000").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+X0000 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+1X000 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+10X00 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+100X0 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+1000X is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+10000X is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+100000X is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+1000000X is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(+1000000000 is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-1000000000").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(-1000000000 is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-0000").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(-0000 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("10000").fromJsonValidation[Year])(
            isSingleFailure(
              equalTo("(10000 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          )
        },
        test("YearMonth") {
          assert(stringify("").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal year month at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020 is not a valid ISO-8601 format, illegal year month at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal year month at index 5)")
            )
          ) &&
          assert(stringify("2020-012").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020-012 is not a valid ISO-8601 format, illegal year month at index 7)")
            )
          ) &&
          assert(stringify("X020-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(X020-01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2X20-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(20X0-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(202X-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020X01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020-X1 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020-0X is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("+X0000-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+X0000-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+1X000-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+10X00-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+100X0-01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+1000X-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+10000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+100000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+1000000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+1000000000-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(-1000000000-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(-0000-01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal year month at index 6)")
            )
          ) &&
          assert(stringify("2020-00").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020-00 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13").fromJsonValidation[YearMonth])(
            isSingleFailure(
              equalTo("(2020-13 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          )
        },
        test("ZonedDateTime") {
          assert(stringify("").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal zoned date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020 is not a valid ISO-8601 format, illegal zoned date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal zoned date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal zoned date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal zoned date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal zoned date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal zoned date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, expected '[' at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal zoned date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, expected '[' at index 22)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal zoned date time at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, expected '[' at index 25)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal zoned date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:01X").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01:01+01:01:01X is not a valid ISO-8601 format, expected '[' at index 28)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(-1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal zoned date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01Z[ is not a valid ISO-8601 format, illegal zoned date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[X]").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01Z[X] is not a valid ISO-8601 format, illegal zoned date time at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[GMT]X").fromJsonValidation[ZonedDateTime])(
            isSingleFailure(
              equalTo("(2020-01-01T01:01Z[GMT]X is not a valid ISO-8601 format, illegal zoned date time at index 22)")
            )
          )
        },
        test("ZoneId") {
          assert(stringify("America/New York").fromJsonValidation[ZoneId])(
            isSingleFailure(equalTo("(America/New York is not a valid ISO-8601 format, illegal zone id at index 0)"))
          ) &&
          assert(stringify("Solar_System/Mars").fromJsonValidation[ZoneId])(
            isSingleFailure(equalTo("(Solar_System/Mars is not a valid ISO-8601 format, illegal zone id at index 0)"))
          )
        },
        test("ZoneOffset") {
          assert(stringify("").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("( is not a valid ISO-8601 format, illegal zone offset at index 0)")
            )
          ) &&
          assert(stringify("X").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 0)")
            )
          ) &&
          assert(stringify("+X1:01:01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+X1:01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+0").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+0 is not a valid ISO-8601 format, illegal zone offset at index 1)")
            )
          ) &&
          assert(stringify("+0X:01:01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+0X:01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+19:01:01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo(
                "(+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 2)"
              )
            )
          ) &&
          assert(stringify("+01X01:01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo(
                "(+01X01:01 is not a valid ISO-8601 format, illegal zone offset at index 4)"
              )
            )
          ) &&
          assert(stringify("+01:0").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+01:0 is not a valid ISO-8601 format, illegal zone offset at index 4)")
            )
          ) &&
          assert(stringify("+01:X1:01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+01:X1:01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+01:0X:01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+01:0X:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+01:60:01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo(
                "(+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 5)"
              )
            )
          ) &&
          assert(stringify("+01:01X01").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo(
                "(+01:01X01 is not a valid ISO-8601 format, illegal zone offset at index 7)"
              )
            )
          ) &&
          assert(stringify("+01:01:0").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo(
                "(+01:01:0 is not a valid ISO-8601 format, illegal zone offset at index 7)"
              )
            )
          ) &&
          assert(stringify("+01:01:X1").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+01:01:X1 is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("+01:01:0X").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo("(+01:01:0X is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("+01:01:60").fromJsonValidation[ZoneOffset])(
            isSingleFailure(
              equalTo(
                "(+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 8)"
              )
            )
          )
        }
      )
    )
}
