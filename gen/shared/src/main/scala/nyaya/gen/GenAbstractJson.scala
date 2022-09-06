package nyaya.gen

import cats.instances.list._
import cats.syntax.all._
import cats.{Applicative, Eval, Traverse}
import japgolly.microlibs.recursion._

object GenAbstractJson {

  sealed trait JsonF[+A]
  object JsonF {
    case object Null                                   extends JsonF[Nothing]
    case object True                                   extends JsonF[Nothing]
    case object False                                  extends JsonF[Nothing]
    final case class Str(value: String)                extends JsonF[Nothing]
    final case class NumDouble(value: Double)          extends JsonF[Nothing]
    final case class NumLong(value: Long)              extends JsonF[Nothing]
    final case class Arr[A](values: List[A])           extends JsonF[A]
    final case class Obj[A](fields: List[(String, A)]) extends JsonF[A]

    implicit val traverse: Traverse[JsonF] = new Traverse[JsonF] {

      override def foldLeft[A, B](fa: JsonF[A], b: B)(f: (B, A) => B): B =
        fa match {
          case Null
             | True
             | False
             | _: Str
             | _: NumLong
             | _: NumDouble => b
          case Arr(values)  => values.foldLeft(b)(f)
          case Obj(fields)  => fields.iterator.map(_._2).foldLeft(b)(f)
        }

      override def foldRight[A, B](fa: JsonF[A], eb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case Null
             | True
             | False
             | _: Str
             | _: NumLong
             | _: NumDouble => eb
          case Arr(values)  => values.foldRight(eb)(f)
          case Obj(fields)  => fields.iterator.map(_._2).foldRight(eb)(f)
        }

      override def traverse[G[_], A, B](fa: JsonF[A])(f: A => G[B])(implicit G: Applicative[G]): G[JsonF[B]] =
        fa match {
          case Null         => G.pure(Null)
          case True         => G.pure(True)
          case False        => G.pure(False)
          case x: Str       => G.pure(x)
          case x: NumLong   => G.pure(x)
          case x: NumDouble => G.pure(x)
          case Arr(values)  => values.traverse(f).map(Arr(_))
          case Obj(fields)  => fields.traverse { case (k, v) => f(v).map((k, _)) }.map(Obj(_))
        }
    }
  }

  // ===================================================================================================================

  final case class Spec(currentDepth: Int, minDepth: Int, maxDepth: Int) {
    def minDepthReached = currentDepth >= minDepth
    def maxDepthReached = currentDepth >= maxDepth
  }

  object Default {
    val genString: Gen[String] = Gen.string
    val maxSize  : Int         = 4
  }

  def generator(genString: Gen[String] = Default.genString,
                maxSize  : Int         = Default.maxSize): FCoalgebraM[Gen, JsonF, Spec] = {

    val genNull      = Gen.pure(JsonF.Null)
    val genTrue      = Gen.pure(JsonF.True)
    val genFalse     = Gen.pure(JsonF.False)
    val genStr       = genString.map(JsonF.Str)
    val genNumDouble = Gen.double.map(JsonF.NumDouble)
    val genNumLong   = Gen.long.map(JsonF.NumLong)

    val nonRecursiveGens: List[Gen[JsonF[Spec]]] =
      List(
        genNull,
        genTrue,
        genFalse,
        genStr,
        genNumDouble,
        genNumLong,
      )

    spec => {
      var gens: List[Gen[JsonF[Spec]]] =
        if (spec.minDepthReached)
          nonRecursiveGens
        else
          Nil

      // recursive cases
      if (!spec.maxDepthReached) {
        val nextLevel = spec.copy(currentDepth = spec.currentDepth + 1)
        val minSize   = if (spec.minDepthReached) 0 else 1
        gens ::= Gen.pure(nextLevel).list(minSize to maxSize).map(JsonF.Arr(_))
        gens ::= genString.map((_, nextLevel)).list(minSize to maxSize).map(JsonF.Obj(_))
      }

      Gen.chooseGen_!(gens)
    }
  }

  // ===================================================================================================================

  trait Dsl[Json, JsonObject] { self =>

    val JsonObject: List[(String, Json)] => JsonObject
    val algebra   : FAlgebra[JsonF, Json]

    val genString : Gen[String] = Default.genString
    val maxSize   : Int         = Default.maxSize

    protected def copy(newGenString : Gen[String] = self.genString,
                       newMaxSize   : Int         = self.maxSize,
                      ): Dsl[Json, JsonObject] =
      new Dsl[Json, JsonObject] {
        override val JsonObject = self.JsonObject
        override val algebra    = self.algebra
        override val genString  = newGenString
        override val maxSize    = newMaxSize
      }

    def withStringGen(g: Gen[String]): Dsl[Json, JsonObject] =
      copy(newGenString = g)

    def withMaxSizePerLevel(n: Int): Dsl[Json, JsonObject] =
      copy(newMaxSize = n)

    lazy val coalgebraGen: FCoalgebraM[Gen, JsonF, Spec] =
      GenAbstractJson.generator(genString, maxSize)

    lazy val algebraGen: FAlgebraM[Gen, JsonF, Json] =
      algebra.toFAlgebraM[Gen]

    /** @param depth ≧ 1 */
    def exactDepth(depth: Int): Gen[Json] =
      apply(depth, depth)

    /** @param maxDepth ≧ 1 */
    def apply(maxDepth: Int): Gen[Json] =
      apply(1, maxDepth)

    /** @param minDepth ≧ 1
      * @param maxDepth ≧ 1
      */
    def apply(minDepth: Int, maxDepth: Int): Gen[Json] = {
      val spec = Spec(1, minDepth, maxDepth)
      Recursion.hyloM(coalgebraGen, algebraGen)(spec)
    }

    /** @param minDepth ≧ 1
      * @param maxDepth ≧ 1
      */
    def obj(minDepth: Int, maxDepth: Int): Gen[JsonObject] =
      Gen.tuple2(genString, this(minDepth, maxDepth)).list(0 to maxSize).map(JsonObject)

    lazy val small: Gen[Json] =
      apply(1, 2)

    lazy val smallObj: Gen[JsonObject] =
      obj(1, 2)
  }

}
