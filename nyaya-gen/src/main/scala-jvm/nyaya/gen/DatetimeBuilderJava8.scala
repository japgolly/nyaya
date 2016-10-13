package nyaya.gen

import java.time._
import DateTimeBuilder._
import DatetimeBuilderJava8._

object DatetimeBuilderJava8 {

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  val avoidJDK8066982: ZonedDateTime => ZonedDateTime =
  d => ZonedDateTime.parse(d.toString)
}

trait DatetimeBuilderJava8 {
  this: DateTimeBuilder =>

  def  fromInstant(i: Instant) =  fromEpochMs(i.toEpochMilli)
  def untilInstant(i: Instant) = untilEpochMs(i.toEpochMilli)

  def  fromZDT(d: ZonedDateTime) =  fromInstant(d.toInstant)
  def untilZDT(d: ZonedDateTime) = untilInstant(d.toInstant)

  def asInstant: Gen[Instant] =
    asEpochMs.map(Instant.ofEpochMilli)

  def asLocalDateTime: Gen[LocalDateTime] = {
    val systemZoneId = ZoneId.systemDefault()
    asInstant.map(LocalDateTime.ofInstant(_, systemZoneId))
  }

  def asZonedDateTime: Gen[ZonedDateTime] =
    asZonedDateTime(Gen.zoneId)

  def asZonedDateTime(genZoneId: Gen[ZoneId]): Gen[ZonedDateTime] =
    Gen.lift2(asInstant, genZoneId)(_ atZone _)

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  def asZonedDateTimeAvoidingJDK8066982: Gen[ZonedDateTime] =
    asZonedDateTime map avoidJDK8066982

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  def asZonedDateTimeAvoidingJDK8066982(genZoneId: Gen[ZoneId]): Gen[ZonedDateTime] =
    asZonedDateTime(genZoneId) map avoidJDK8066982
}
