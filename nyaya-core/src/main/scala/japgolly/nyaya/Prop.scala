package japgolly.nyaya

import scala.annotation.elidable
import scala.collection.GenTraversable
import scalaz.{Need, Equal, Foldable, Contravariant}
import scalaz.syntax.equal._
import scalaz.syntax.foldable._

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

  def atom[A](name: => String, t: A => FailureReasonO): Prop[A] =
    eval(a => Eval.atom(name, a, t(a)))

  def eval[A](q: A => EvalL): Prop[A] =
    Atom[PropA, A](new PropA(q))

  def run[A](l: Prop[A])(a: A): Eval =
    l.run(p => Eval.run(p.t(a)))

  def test[A](name: => String, t: A => Boolean): Prop[A] =
    atom(name, a => reasonBool(t(a), a))

  def equalSelf[A: Equal](name: => String, f: A => A): Prop[A] =
    equal[A, A](name, f, identity)

  def equal[A, B: Equal](name: => String, actual: A => B, expect: A => B): Prop[A] =
    atom[A](name, a => reasonEq(actual(a), expect(a)))

  def equal[A](name: => String) = new EqualB[A](name)
  final class EqualB[A](val name: String) extends AnyVal {
    def apply[B: Equal](actual: A => B, expect: A => B): Prop[A] = equal(name, actual, expect)
  }

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
    eval[A](a => {
      val p  = prop(a)
      val es = f(a).foldLeft(List.empty[Eval])((q, b) => run(p)(b) :: q)
      val ho = es.headOption
      val n  = Need(ho.fold("∅")(e => s"∀{${e.name.value}}"))
      val i  = Input(a)
      val r  = es.filter(_.failure) match {
                 case Nil =>
                   Eval.success(n, i)
                 case fs@(_ :: _) =>
                   val causes = fs.foldLeft(Eval.root)((q, e) => q.add(e.name.value, List(e)))
                   Eval(n, i, causes)
               }
      r.liftL
    })

  def distinctC[C[_], A](name: => String)(implicit ev: C[A] <:< GenTraversable[A]): Prop[C[A]] =
    distinct(name, _.toStream)

  def distinct[A, B](name: => String, f: A => GenTraversable[B]): Prop[A] =
    distinct[B](name).contramap(f(_).toStream)

  def distinct[A](name: => String): Prop[Stream[A]] =
    eval(as => Eval.distinct(name, as, as))

  /**
   * Test that all of A's Cs are on a whitelist.
   */
  @inline def whitelist[A](name: String) = new WhitelistB[A](name)
  final class WhitelistB[A](val name: String) extends AnyVal {
    def apply[B, C](whitelist: A => Set[B], testData: A => Traversable[C])(implicit ev: C <:< B): Prop[A] =
      setTest(name, true, "Whitelist", whitelist, "Found    ", testData, "Illegal  ")
  }

  /**
   * Test that none of A's Cs are on a blacklist.
   */
  @inline def blacklist[A](name: String) = new BlacklistB[A](name)
  final class BlacklistB[A](val name: String) extends AnyVal {
    def apply[B, C](blacklist: A => Set[B], testData: A => Traversable[C])(implicit ev: C <:< B): Prop[A] =
      setTest(name, false, "Blacklist", blacklist, "Found    ", testData, "Illegal  ")
  }

  /**
   * Test that all (A's) Bs are present in A's Cs.
   */
  @inline def allPresent[A](name: String) = new AllPresentB[A](name)
  final class AllPresentB[A](val name: String) extends AnyVal {
    def apply[B, C](requiredSubset: A => Set[B], testData: A => Traversable[C])(implicit ev: B <:< C): Prop[A] =
      atom[A](name, a => {
        val bs  = requiredSubset(a)
        val cs1 = testData(a)
        val cs2 = cs1.toSet
        val rs = bs.filterNot(cs2 contains _)
        setMembershipResult(a, "Required", bs, "Found   ", cs1, "Missing ", rs)
      })
  }

  private[this] def fmtSet(s: Set[_]): String =
    s.toStream.map(_.toString).sorted.distinct.mkString("{", ", ", "}")

  private[this] def setTest[A, B, C](name: String, expect: Boolean,
                                     bsName: String, getBs: A => Set[B],
                                     csName: String, getCs: A => Traversable[C],
                                     failureName: String)(implicit ev: C <:< B): Prop[A] =
    atom[A](name, a => {
      val bs = getBs(a)
      val cs = getCs(a)
      val rs = cs.foldLeft(Set.empty[C])((q, c) => if (bs.contains(c) == expect) q else q + c)
        setMembershipResult(a, bsName, bs, csName, cs, failureName, rs)
    })



  private[this] def setMembershipResult(input: Any,
                                        asName: String, as: Traversable[_],
                                        bsName: String, bs: Traversable[_],
                                        failureName: String, problems: Set[_]): FailureReasonO =
    if (problems.isEmpty)
      None
    else
      Some(s"$input\n$asName: (${as.size}) $as\n$bsName: (${bs.size}) $bs\n$failureName: ${fmtSet(problems)}")
}
