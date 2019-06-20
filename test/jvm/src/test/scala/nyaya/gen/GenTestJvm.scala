package nyaya.gen

import java.time._
import scala.concurrent.duration._
import scala.collection.SortedSet
import scalaz.std.AllInstances._
import utest._
import nyaya.prop._
import nyaya.test.ParallelExecutor
import nyaya.test.PropTest._
import DateTimeBuilderJava8.UTC
import Gen.Now

object GenTestJvm extends TestSuite {

  override def tests = Tests {

    "dateTime" - {

      // scala> Instant.ofEpochMilli(1476661717639L).atZone(ZoneId of "UTC")
      // res1: java.time.ZonedDateTime = 2016-10-16T23:48:37.639Z[UTC]
      val now = Now(1476661717639L)
      val nowUTC = Instant.ofEpochMilli(now.millisSinceEpoch).atZone(UTC).toLocalDate
      implicit val genNow = Gen pure now

      def testDeltaDayRange(b: DateTimeBuilder, is: Iterable[Int]): Unit = {
        val g = b.asZonedDateTime(UTC)
        val results = g.samples().take(is.size * 2048).map(_.toLocalDate.toString).to(SortedSet)
        val expect = is.map(nowUTC.plusDays(_).toString).to(SortedSet)
        assert(results == expect)
      }

      "deltaPast" -
        testDeltaDayRange(Gen.dateTime.fromNowMinus(3.days).untilNow, -3 to 0)

      "deltaFuture" -
        testDeltaDayRange(Gen.dateTime.fromNow.untilNowPlus(3.days), 0 to 3)

      "deltaAround" -
        testDeltaDayRange(Gen.dateTime.fromNowMinus(7.days).untilNowPlus(5.days), -7 to 5)

      "zonedDateTime" - {
        val used = Gen.dateTime.asZonedDateTime.samples().take(100).map(_.getZone).toSet
        assert(used.size > 1)
      }

      "zonedDateTimeZ" - {
        val zoneId = Gen.zoneId.sample()
        val used = Gen.dateTime.asZonedDateTime(zoneId).samples().take(100).map(_.getZone).toSet
        assert(used == Set(zoneId))
      }

      "zonedDateTimeG" - {
        val zoneIds = Gen.zoneId.sizedSet(3).sample()
        val used = Gen.dateTime.asZonedDateTime(Gen choose_! zoneIds).samples().take(100).map(_.getZone).toSet
        assert(used == zoneIds)
      }

      "zonedDateTimeAvoidingJDK8066982" - {
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

      "deterministic" - {
        val g: ZonedDateTime = Gen.dateTime.aroundNowDays(8).asZonedDateTime(UTC).withSeed(0).sample()
        assert(g.toString == "2016-10-14T23:17:45.668Z[UTC]")
      }

    }

    "parallel" - {
      def settings = defaultPropSettings.copy(executor = ParallelExecutor(2)).setSampleSize(4)
      val lock = new AnyRef
      var seen = Vector.empty[Int]
      val prop = Prop.atom[Int]("", i => {
        lock.synchronized(seen :+= i)
        None
      })
      def results() = lock.synchronized(seen)

      "noSeed" - {
        Gen.int.mustSatisfy(prop)(settings)
        val r = results()
        assert(r.toSet.size == r.size) // no duplicates
      }

      "withSeed" - {
        Gen.int.withSeed(0).mustSatisfy(prop)(settings)
        val r = results()
        assert(r.toSet.size == r.size) // no duplicates
      }

      "withConstSeed" - {
        Gen.int.withConstSeed(0).mustSatisfy(prop)(settings)
        assert(results().toSet.size == 1)
      }
    }

  }
}
