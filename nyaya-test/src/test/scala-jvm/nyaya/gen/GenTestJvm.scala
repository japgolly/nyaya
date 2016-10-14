package nyaya.gen

import java.nio.charset.Charset
import java.time.format.DateTimeFormatter
import java.time._
import scala.concurrent.duration._
import scalaz.{-\/, BindRec, NonEmptyList, \/-}
import scalaz.std.AllInstances._
import utest._
import nyaya.prop._
import nyaya.test.PropTest._
import DateTimeBuilderJava8.UTC

object GenTestJvm extends TestSuite {

  override def tests = TestSuite {

    'dateTime {

      val now = ZonedDateTime.now(UTC).toLocalDateTime

      def testDeltaDayRange(b: DateTimeBuilder, is: TraversableOnce[Int]): Unit = {
        val g = b.asLocalDateTime
        val results = g.samples().take(is.size * 50).map(_.toLocalDate).toSet
        val expect = is.map(now.plusDays(_).toLocalDate).toSet
        assert(results == expect)
      }

      'deltaPast -
        testDeltaDayRange(Gen.dateTime.fromNowMinus(3.days).untilNow, -3 to 0)

      'deltaFuture -
        testDeltaDayRange(Gen.dateTime.fromNow.untilNowPlus(3.days), 0 to 3)

      'deltaAround -
        testDeltaDayRange(Gen.dateTime.fromNowMinus(7.days).untilNowPlus(5.days), -7 to 5)

      'zonedDateTime {
        val used = Gen.dateTime.asZonedDateTime.samples().take(100).map(_.getZone).toSet
        assert(used.size > 1)
      }

      'zonedDateTimeZ {
        val zoneId = Gen.zoneId.sample()
        val used = Gen.dateTime.asZonedDateTime(zoneId).samples().take(100).map(_.getZone).toSet
        assert(used == Set(zoneId))
      }

      'zonedDateTimeG {
        val zoneIds = Gen.zoneId.sizedSet(3).sample()
        val used = Gen.dateTime.asZonedDateTime(Gen choose_! zoneIds).samples().take(100).map(_.getZone).toSet
        assert(used == zoneIds)
      }

      'zonedDateTimeAvoidingJDK8066982 {
        /*
        val gen: Gen[(Long, ZoneId)] = Gen.time.aroundNow(365.days * 30).genEpochMs *** Gen.zoneId
  //      val f: ((Long, ZoneId)) => ZonedDateTime = x => Instant.ofEpochMilli(x._1).atZone(x._2)
        val f: ((Long, ZoneId)) => ZonedDateTime = x => Instant.ofEpochMilli(x._1)atZone(ZoneId of "UTC") withZoneSameLocal x._2
  //      val g = (a: ZonedDateTime) => a.toString
        val g = (a: ZonedDateTime) => a.format(DateTimeFormatter.ISO_ZONED_DATE_TIME)
        val prop: Prop[(Long, ZoneId)] = Prop.equal[(Long, ZoneId)]("toString = toString . parse . toString")(
            g compose f, d => g(ZonedDateTime.parse(g(f(d)))))
        gen.mustSatisfy(prop)(defaultPropSettings.setDebug.setSampleSize(1000000))
        */
        val gen: Gen[ZonedDateTime] = Gen.dateTime.aroundNow(365.days * 30).asZonedDateTimeAvoidingJDK8066982
        val prop: Prop[ZonedDateTime] = Prop.equal[ZonedDateTime]("toString = toString . parse . toString")(
            _.toString, d => ZonedDateTime.parse(d.toString).toString)
        gen.mustSatisfy(prop)//(defaultPropSettings.setDebug.setSampleSize(1000000))
      }
    }

  }
}
