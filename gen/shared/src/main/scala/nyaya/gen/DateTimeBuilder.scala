package nyaya.gen

import java.util.Date
import scala.concurrent.duration.FiniteDuration
import DateTimeBuilder._
import Gen.Now

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
}

final class DateTimeBuilder(genNow: Gen[Now], past: TimeSpec, future: TimeSpec) extends DateTimeBuilderJava8 {

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
    withNowGen(Gen point now)

  def withNowMs(nowMs: => Long): DateTimeBuilder =
    withNowGen(Gen point Now(nowMs))

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
}
