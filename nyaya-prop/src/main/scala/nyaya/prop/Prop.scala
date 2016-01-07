package nyaya.prop

import scala.annotation.elidable
import scala.collection.GenTraversable
import scalaz.{\/, Equal, Foldable, Contravariant, Need}
import scalaz.syntax.equal._

final class PropA[A] private[nyaya](val t: A => EvalL)

object PropA {
  implicit val propInstances: Contravariant[PropA] =
    new Contravariant[PropA] {
      override def contramap[A, B](r: PropA[A])(f: B => A): PropA[B] = new PropA[B](b => r.t(f(b)))
    }
}

object Prop {

  def pass[A](name: String = "Pass"): Prop[A] =
    test(name, _ => true)

  def fail[A](name: => String, reason: String): Prop[A] =
    evaln(name, a => Eval.fail(name, reason, a))

  def atom[A](name: => String, t: A => FailureReasonO): Prop[A] =
    evaln(name, a => Eval.atom(name, a, t(a)))

  def eval[A](q: A => EvalL): Prop[A] =
    Atom[PropA, A](None, new PropA(q))

  def evaln[A](name: => String, q: A => EvalL): Prop[A] =
    Atom[PropA, A](Some(Need(name)), new PropA(q))

  def run[A](l: Prop[A])(a: A): Eval =
    l.run(p => Eval.run(p.t(a)))

  def test[A](name: => String, t: A => Boolean): Prop[A] =
    atom(name, a => reasonBool(t(a), a))

  def equalSelf[A: Equal](name: => String, f: A => A): Prop[A] =
    equal[A, A](name, f, identity)

  def equal[A, B: Equal](name: => String, actual: A => B, expect: A => B): Prop[A] =
    atom[A](name, a => reasonEq(actual(a), expect(a)))

  def equal[A](name: => String) = new EqualB[A](name)
  final class EqualB[A](private val name: String) extends AnyVal {
    def apply[B: Equal](actual: A => B, expect: A => B): Prop[A] = equal(name, actual, expect)
  }

  def either[A, B](name: => String, f: A => String \/ B)(p: Prop[B]): Prop[A] =
    evaln(name, a => Eval.either(name, a, f(a))(p(_).liftL))

  def reason(b: Boolean, r: => String): FailureReasonO =
    if (b) None else Some(r)

  def reasonBool(b: Boolean, input: => Any): FailureReasonO =
    reason(b, s"Invalid input [$input]")

  def reasonEq[A: Equal](a: A, e: A): FailureReasonO =
    reason(a ≟ e, s"Actual: $a\nExpect: $e")

  @elidable(elidable.ASSERTION)
  def assert[A](l: => Prop[A])(a: => A): Unit =
    l(a).assertSuccess()

  def forall[A, F[_]: Foldable, B](f: A => F[B])(prop: A => Prop[B]): Prop[A] =
    forallS(f)(prop)

  def forallS[A, F[_]: Foldable, B, C](f: A => F[B])(prop: A => Prop[C])(implicit ev: B <:< C): Prop[A] =
    eval { a =>
      val p = prop(a)
      Eval.forall(a, f(a))(p(_).liftL)
    }

  def distinctC[C[_], A](name: => String)(implicit ev: C[A] <:< GenTraversable[A]): Prop[C[A]] =
    distinct(name, ev)

  def distinct[A, B](name: => String, f: A => GenTraversable[B]): Prop[A] =
    distinct[B](name).contramap(f)

  def distinct[A](name: => String): Prop[GenTraversable[A]] =
    evaln(Eval distinctName name, as => Eval.distinct(name, as, as))

  /**
   * Test that all of A's Cs are on a whitelist.
   */
  @inline def whitelist[A](name: String) = new WhitelistB[A](name)
  final class WhitelistB[A](private val name: String) extends AnyVal {
    def apply[B, C](whitelist: A => Set[B], testData: A => Traversable[C])(implicit ev: C <:< B): Prop[A] =
      evaln(s"$name whitelist", a => Eval.whitelist(name, a, whitelist(a), testData(a)))
  }

  /**
   * Test that none of A's Cs are on a blacklist.
   */
  @inline def blacklist[A](name: String) = new BlacklistB[A](name)
  final class BlacklistB[A](private val name: String) extends AnyVal {
    def apply[B, C](blacklist: A => Set[B], testData: A => Traversable[C])(implicit ev: C <:< B): Prop[A] =
      evaln(s"$name blacklist", a => Eval.blacklist(name, a, blacklist(a), testData(a)))
  }

  /**
   * Test that all (A's) Bs are present in A's Cs.
   */
  @inline def allPresent[A](name: String) = new AllPresentB[A](name)
  final class AllPresentB[A](private val name: String) extends AnyVal {
    def apply[B, C](requiredSubset: A => Set[B], testData: A => Traversable[C])(implicit ev: B <:< C): Prop[A] =
      evaln(s"$name allPresent", a => Eval.allPresent(name, a, requiredSubset(a), testData(a)))
  }
}
