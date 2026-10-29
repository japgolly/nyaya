package nyaya.gen

import java.time._
import java.util.Date
import nyaya.gen.DateTimeBuilder._
import nyaya.gen.Gen.Now
import scala.concurrent.duration.FiniteDuration

object DateTimeBuilder {

  sealed abstract class TimeSpec {
    final type Spec = Either[Long, Long => Long]
    protected def fixed(l: Long): Spec = Left(l)
    protected def fn(f: Long => Long): Spec = Right(f)
    val past, future: Spec
  }
  case class Delta(ms: Long) extends TimeSpec {
    override val past = fn(_ - ms)
    override val future = fn(_ + ms)
  }
  case class Fixed(epochMs: Long) extends TimeSpec {
    override val past = fixed(epochMs)
    override val future = past
  }
  case object Unlimited extends TimeSpec {
    override val past = fixed(0)
    override val future = fixed(Long.MaxValue - 1)
  }

  def default(implicit genNow: Gen[Now]): DateTimeBuilder =
    new DateTimeBuilder(genNow, Unlimited, Unlimited)

  val DayMs = 86400000L.toDouble
  val YearMs = DayMs * 365.25
  val MonthMs = YearMs / 12
  val WeekMs = YearMs / 52

  val UTC = ZoneId.of("UTC")

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  val avoidJDK8066982: ZonedDateTime => ZonedDateTime =
    d => ZonedDateTime.parse(d.toString)
}

final class DateTimeBuilder(genNow: Gen[Now], past: TimeSpec, future: TimeSpec) {

  protected def copy(genNow: Gen[Now] = genNow, past: TimeSpec = past, future: TimeSpec = future): DateTimeBuilder =
    new DateTimeBuilder(genNow, past = past, future = future)

  def fromEpochMs(e: Long)            = copy(past = Fixed(e))
  def fromNowMinusMs(d: Long)         = copy(past = Delta(d))
  def fromDate(d: Date)               = fromEpochMs(d.getTime)
  def fromNow                         = fromNowMinusMs(0)
  def fromNowMinus(d: FiniteDuration) = fromNowMinusMs(d.toMillis)
  def fromNowMinusYears(d: Double)    = fromNowMinusMs((YearMs * d).toLong)
  def fromNowMinusMonths(d: Double)   = fromNowMinusMs((MonthMs * d).toLong)
  def fromNowMinusWeeks(d: Double)    = fromNowMinusMs((WeekMs * d).toLong)
  def fromNowMinusDays(d: Double)     = fromNowMinusMs((DayMs * d).toLong)

  def  fromInstant(i: Instant) =  fromEpochMs(i.toEpochMilli)
  def untilInstant(i: Instant) = untilEpochMs(i.toEpochMilli)

  def  fromZDT(d: ZonedDateTime) =  fromInstant(d.toInstant)
  def untilZDT(d: ZonedDateTime) = untilInstant(d.toInstant)

  def untilEpochMs(e: Long)           = copy(future = Fixed(e))
  def untilNowPlusMs(d: Long)         = copy(future = Delta(d))
  def untilDate(d: Date)              = untilEpochMs(d.getTime)
  def untilNow                        = untilNowPlusMs(0)
  def untilNowPlus(d: FiniteDuration) = untilNowPlusMs(d.toMillis)
  def untilNowPlusYears(d: Double)    = untilNowPlusMs((YearMs * d).toLong)
  def untilNowPlusMonths(d: Double)   = untilNowPlusMs((MonthMs * d).toLong)
  def untilNowPlusWeeks(d: Double)    = untilNowPlusMs((WeekMs * d).toLong)
  def untilNowPlusDays(d: Double)     = untilNowPlusMs((DayMs * d).toLong)

  def aroundNow(d: FiniteDuration) = fromNowMinus(d).untilNowPlus(d)
  def aroundNowMs(d: Long)         = fromNowMinusMs(d).untilNowPlusMs(d)
  def aroundNowDays(d: Double)     = fromNowMinusDays(d).untilNowPlusDays(d)
  def aroundNowMonths(d: Double)   = fromNowMinusMonths(d).untilNowPlusMonths(d)
  def aroundNowWeeks(d: Double)    = fromNowMinusWeeks(d).untilNowPlusWeeks(d)
  def aroundNowYears(d: Double)    = fromNowMinusYears(d).untilNowPlusYears(d)

  def withNowGen(g: Gen[Now]): DateTimeBuilder =
    copy(genNow = g)

  def withNow(now: => Now): DateTimeBuilder =
    withNowGen(Gen.delay(now))

  def withNowMs(nowMs: => Long): DateTimeBuilder =
    withNowGen(Gen.delay(Now(nowMs)))

  /** The current time is sampled once and reused */
  def withNowSampledOnce: DateTimeBuilder =
    withNowGen(Now.genNowOnce)

  /** The current time is resampled every time it is needed */
  def withNowLive: DateTimeBuilder =
    withNowGen(Now.genNowByName)

  // ===================================================================================================================

  lazy val asEpochMs: Gen[Long] = {
    def specToFn(s: TimeSpec#Spec): Long => Long = s.fold(Function const, identity)
    (past.past, future.future) match {
      case (Left(a), Left(b)) =>
        Gen.chooseLong(a, b)
      case (x, y) =>
        val a = specToFn(x)
        val b = specToFn(y)
        genNow.flatMap(now => Gen.chooseLong(a(now.millisSinceEpoch), b(now.millisSinceEpoch)))
    }
  }

  def asDate: Gen[Date] =
    asEpochMs.map(new Date(_))

  def asInstant: Gen[Instant] =
    asEpochMs.map(Instant.ofEpochMilli)

  def asLocalDateTime: Gen[LocalDateTime] =
    asInstant.map(LocalDateTime.ofInstant(_, UTC))

  def asZonedDateTime: Gen[ZonedDateTime] =
    asZonedDateTime(Gen.zoneId)

  def asZonedDateTime(zoneId: ZoneId): Gen[ZonedDateTime] =
    asInstant.map(_ atZone zoneId)

  def asZonedDateTime(genZoneId: Gen[ZoneId]): Gen[ZonedDateTime] =
    Gen.lift2(asInstant, genZoneId)(_ atZone _)

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  def asZonedDateTimeAvoidingJDK8066982: Gen[ZonedDateTime] =
    asZonedDateTime map avoidJDK8066982

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  def asZonedDateTimeAvoidingJDK8066982(zoneId: ZoneId): Gen[ZonedDateTime] =
    asZonedDateTime(zoneId) map avoidJDK8066982

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  def asZonedDateTimeAvoidingJDK8066982(genZoneId: Gen[ZoneId]): Gen[ZonedDateTime] =
    asZonedDateTime(genZoneId) map avoidJDK8066982
}
