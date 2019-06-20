package nyaya.util

import scalaz.{Order, Equal}
import scalaz.std.AllInstances._
import utest._
import nyaya.gen._
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import Multimap.Internal._
import MultiValues.Commutative

object MultimapTest extends TestSuite {

  // Here we are going to test Multimap
  case class PropInputs[L[_] : MultiValues, A](mm: Multimap[A, L, A], a: A, b: A, as: L[A],
                                               commutative: Option[Commutative[L]])
                                              (implicit A: Order[A], L: Equal[L[A]]) {

    // This is a helper that facilitates easy creation, composition & logic
    val E = EvalOver(this)

    // Helper function for a quick L[A] of just the given A
    def l(x: A): L[A] = el add1 x

    // Let's cache a few repeated values that we will use throughout the test
         val em    = Multimap.empty[A, L, A]
         val el    = MultiValues[L].empty[A]
    lazy val ab    = l(a) add1 b
    lazy val bigas = as add1 a add1 b
    lazy val bigm  = mm.addks(bigas, a).addvs(b, bigas)

    // This will test that two operations (f and g) commute
    type MM = Multimap[A, L, A]
    def comm[R: Equal](name: => String, z: MM, r: MM => R, f: MM => MM, g: MM => MM) =
      E.equal(name, r(f(g(z))), r(g(f(z))))

    // And now the propositions begin...
    // First the propositions that are only tested when L is commutative
    def commutativeProps = commutative.fold(E.pass)(c => {
      implicit def cc = c
      ( E.equal("reverse.reverse = id"      , mm.reverse.reverse                     , mm)
      ∧ E.equal("addvs symmetrical to addks", mm.addvs(a, as).reverse                , mm.reverse.addks(as, a))
      ∧ E.equal("delvs symmetrical to delks", bigm.delvs(ab).reverse                 , bigm.reverse.delks(ab))
      ∧ E.equal("delv symmetrical to delk",   bigm.delv(b).reverse                   , bigm.reverse.delk(b))
      ∧ E.equal("get⁻¹.setks.add = ks",       em.add(b, a).setks(as, a).reverse(c)(a), as)
      )
    })

    // Then all the other propositions.
    def eval =
    ( commutativeProps
    ∧ E.equal("get.setvs.add = vs",         em.add(a, b).setvs(a, as)(a), as)
    ∧ E.equal("get.delk.add = ∅",           em.add(a, b).delk(a)(a)     , el)
    ∧ E.equal("get.add.delk = v",           em.delk(a).add(a, b)(a)     , l(b))
    ∧ E.equal("get.delk.addvs = ∅",         em.addvs(a, as).delk(a)(a)  , el)
    ∧ E.equal("get.addvs.delk = vs",        em.delk(a).addvs(a, as)(a)  , as)
    ∧ E.equal("get₁.delk₂.addvs₁ = vs",     em.addvs(a, as).delk(b)(a)  , as)
    ∧ E.equal("delk.delk = delks",          bigm.delk(a).delk(b)        , bigm.delks(ab))
    ∧ E.equal("delkv == delv.delk",         bigm.delk(a).delv(a)        , bigm.delkv(a))
    ∧ E.equal("unlink == del(kv).del(vk)",  bigm.del(a, b).del(b, a)    , bigm.unlink(a, b))
    ∧ comm("addvs.add ⊇⊆ add.addvs", em, _(a).set, _.addvs(a, as), _.add(a, b))
    ∧ comm("addvs₁₂ ⊇⊆ addvs₂₁",     em, _(a).set, _.addvs(a, as), _.addvs(a, l(b)))
    ) rename "Multimap"
  }

  implicit def commutativeO[L[_]](implicit c: Commutative[L] = null): Option[Commutative[L]] = Option(c)

  def gen[L[_] : MultiValues, A: Order](ga: Gen[A], gl: Gen[A] => Gen[L[A]])
                                       (implicit c: Option[Commutative[L]], E: Equal[L[A]]): Gen[PropInputs[L, A]] = {
    val gla = gl(ga)
    for {
      kvs <- Gen.tuple2(ga, gla).list
      mm  = Multimap(kvs.toMap)
      a   <- ga
      b   <- ga
      as  <- gla
    } yield PropInputs[L, A](mm, a, b, as, c)
  }

  val genSet   : Gen[PropInputs[Set,    Int ]] = gen(Gen.int, _.set)
  val genVector: Gen[PropInputs[Vector, Long]] = gen(Gen.long, _.vector)
  val genList  : Gen[PropInputs[List,   Int ]] = gen(Gen.int, _.list)

  override def tests = TestSuite {
    'list   - genList  .mustSatisfyE(_.eval)
    'set    - genSet   .mustSatisfyE(_.eval)
    'vector - genVector.mustSatisfyE(_.eval)
  }
}