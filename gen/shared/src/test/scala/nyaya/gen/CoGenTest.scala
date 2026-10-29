package nyaya.gen

import java.util.UUID
import scala.collection.immutable.ArraySeq
import scala.util.Try
import utest._

object CoGenTest extends TestSuite {

  override def tests = Tests {

    "implicits" - {
      def assertImplicitExists[A: CoGen] = ()

      "byte" - assertImplicitExists[Byte]
      "char" - assertImplicitExists[Char]
      "double" - assertImplicitExists[Double]
      "exception" - assertImplicitExists[Exception]
      "float" - assertImplicitExists[Float]
      "int" - assertImplicitExists[Int]
      "long" - assertImplicitExists[Long]
      "short" - assertImplicitExists[Short]
      "string" - assertImplicitExists[String]
      "throwable" - assertImplicitExists[Throwable]
      "unit" - assertImplicitExists[Unit]
      "uuid" - assertImplicitExists[UUID]

      "array" - assertImplicitExists[Array[Int]]
      "arraySeq" - assertImplicitExists[ArraySeq[Int]]
      "indexedSeq" - assertImplicitExists[IndexedSeq[Int]]
      "list" - assertImplicitExists[List[Int]]
      "map" - assertImplicitExists[Map[Int, Int]]
      "seq" - assertImplicitExists[Seq[Int]]
      "vector" - assertImplicitExists[Vector[Int]]

      "either" - assertImplicitExists[Either[Int, Int]]
      "option" - assertImplicitExists[Option[Int]]
      "try" - assertImplicitExists[Try[Int]]
    }

    "functionGeneration" - {
      val (f, g) = Gen.long.function1[Int].pair.sample()
      assert(f(1) == f(1))
      assert(f(2) == f(2))
      assert(f(1) != f(2))
      assert(f(1) != g(1))
      assert(g(1) == g(1))
      assert(g(2) == g(2))
    }

  }
}
