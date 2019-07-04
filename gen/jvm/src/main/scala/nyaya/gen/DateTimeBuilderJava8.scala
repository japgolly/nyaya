package nyaya.gen

import java.time._
import DateTimeBuilderJava8._

object DateTimeBuilderJava8 {

  val UTC = ZoneId.of("UTC")

  /** http://stackoverflow.com/questions/40010089/zoneddatetime-parse-bug */
  val avoidJDK8066982: ZonedDateTime => ZonedDateTime =
  d => ZonedDateTime.parse(d.toString)
}

trait DateTimeBuilderJava8 {
  this: DateTimeBuilder =>

  def  fromInstant(i: Instant) =  fromEpochMs(i.toEpochMilli)
  def untilInstant(i: Instant) = untilEpochMs(i.toEpochMilli)

  def  fromZDT(d: ZonedDateTime) =  fromInstant(d.toInstant)
  def untilZDT(d: ZonedDateTime) = untilInstant(d.toInstant)

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
