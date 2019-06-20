package nyaya.prop

import scala.annotation.elidable
import scala.collection.GenTraversable
import scala.collection.mutable
import scalaz._
import scalaz.syntax.foldable._
import nyaya.util.Multimap
import nyaya.util.Util
import scala.collection.compat._

object Eval {
  type Failures = Multimap[FailureReason, List, List[Eval]]
  private[nyaya] val root = Multimap.empty[FailureReason, List, List[Eval]]

  implicit val evalInstances: Contravariant[Eval_] =
    new Contravariant[Eval_] {
      override def contramap[A, B](r: Eval_[A])(f: B => A): Eval_[B] = r
    }

  private[nyaya] def success(name: Name, i: Input): Eval =
    Eval(name, i, root)

  private[nyaya] def rootFail(name: Name, i: Input, failure: FailureReason): Eval =
    Eval(name, i, root.add(failure, Nil))

  def run(l: Logic[Eval_, _]): Eval =
    l.run(identity)

  // -------------------------------------------------------------------------------------------------------------------
  // Logic

  def pass(name: String = "Pass", input: Any = ()): EvalL =
    test(name, input, true)

  def fail(name: => String, reason: String, input: Any = ()): EvalL =
    atom(name, input, Some(reason))

  def atom(name: => String, input: Any, failure: FailureReasonO): EvalL = {
    val n = Need(name)
    Atom[Eval_, Nothing](Some(n), Eval(n, Input(input), failure.fold(root)(root.add(_, Nil))))
  }

  def test(name: => String, input: Any, t: Boolean): EvalL =
    atom(name, input, Prop.reasonBool(t, input))

  def equal[A: Equal](name: => String, input: Any, actual: A, expect: A): EvalL =
    atom(name, input, Prop.reasonEq(actual, expect))

  def equal[A](name: => String, a: A) = new EqualB[A](name, a)
  final class EqualB[A](name: String, a: A)  {
    def apply[B: Equal](actual: A => B, expect: A => B): EvalL = equal(name, a, actual(a), expect(a))
  }

  def either[A](name: => String, input: Any, data: String \/ A)(f: A => EvalL): EvalL =
    data.fold(fail(name, _, input), f)

  @elidable(elidable.ASSERTION)
  def assert(l: => EvalL): Unit =
    run(l).assertSuccess()

  def forall[F[_]: Foldable, B](input: Any, fb: F[B])(each: B => EvalL): EvalL = {
    val es = fb.foldLeft(List.empty[Eval])((q, b) => run(each(b)) :: q)
    val ho = es.headOption
    val n  = Need(ho.fold("∅")(e => s"∀{${e.name.value}}"))
    val i  = Input(input)
    val r  = es.filter(_.failure) match {
      case Nil =>
        Eval.success(n, i)
      case fs@(_ :: _) =>
        val causes = fs.foldLeft(Eval.root)((q, e) => q.add(e.name.value, List(e)))
        Eval(n, i, causes)
    }
    r.liftL
  }

  def distinctName(name: => String) = s"each $name is unique"

  def distinctI[A](name: => String, input: Any, as: Iterator[A]): EvalL =
    distinct(name, input, as.toList)

  def distinct[A](name: => String, input: Any, as: GenTraversable[A]): EvalL =
    atom(distinctName(name), input, {

      val m = mutable.HashMap.empty[A, Int]
      for (a <- as) {
        val v = m.getOrElse(a, 0) + 1
        m.update(a, v)
      }

      if (m.valuesIterator.forall(_ == 1))
        None
      else
        Some {
          val d = m.iterator
            .filter(_._2 > 1)
            .toList
            .sortBy(_._1.toString)
            .map { case (a, i) => s"$a → $i" }
            .mkString("{", ", ", "}")
          s"Inputs: $as\nDups: $d"
        }
    })

  /**
   * Test that all Cs are on a whitelist.
   */
  def whitelist[B, C](name: => String, input: Any, whitelist: Set[B], testData: => IterableOnce[C])(implicit ev: C <:< B): EvalL =
      setTest(name, input, true, "Whitelist", whitelist, "Found    ", testData, "Illegal  ")

  /**
   * Test that no Cs are on a blacklist.
   */
  def blacklist[B, C](name: => String, input: Any, blacklist: Set[B], testData: => IterableOnce[C])(implicit ev: C <:< B): EvalL =
      setTest(name, input, false, "Blacklist", blacklist, "Found    ", testData, "Illegal  ")

  /**
   * Test that all Bs are present in Cs.
   */
  def allPresent[B, C](name: => String, input: Any, required: Set[B], testData: => IterableOnce[C])(implicit ev: B <:< C): EvalL = {
    val cs = testData.toSet
    atom(name, input, {
      val rs = required.filterNot(cs contains _)
      setMembershipResult(input, "Required", required, "Found   ", testData, "Missing ", rs)
    })
  }

  private[this] def setTest[A, B, C](name: => String, input: Any, expect: Boolean,
                                     bsName: String, bs: Set[B],
                                     csName: String, cs: => IterableOnce[C],
                                     failureName: String)(implicit ev: C <:< B): EvalL =
    atom(name, input, {
      val rs = cs.foldLeft(Set.empty[C])((q, c) => if (bs.contains(c) == expect) q else q + c)
      setMembershipResult(input, bsName, bs, csName, cs, failureName, rs)
    })

  private[this] def setMembershipResult(input: Any,
                                        asName: String, as: => IterableOnce[_],
                                        bsName: String, bs: => IterableOnce[_],
                                        failureName: String, problems: Set[_]): FailureReasonO =
    if (problems.isEmpty)
      None
    else
      Some {
        def fmt(name: String, vs: IterableOnce[_]) = {
          val x = vs.toIterable
          s"$name: (${x.size}) $x"
        }
        s"$input\n${fmt(asName, as)}\n${fmt(bsName, bs)}\n$failureName: ${fmtSet(problems)}"
      }

  private[this] def fmtSet(s: Set[_]): String =
    s.iterator.map(_.toString).toList.sorted.distinct.mkString("{", ", ", "}")
}

// =====================================================================================================================
import Eval.Failures

final case class Eval private[nyaya] (name: Name, input: Input, failures: Failures) {
  def rename(n: Name)        : Eval    = rename(_ => n)
  def rename(f: Name => Name): Eval    = copy(name = f(name))
  def success                : Boolean = failures.isEmpty
  def failure                : Boolean = !success
  def liftL                  : EvalL   = Atom[Eval_, Nothing](Some(name), this)

  lazy val reasonsAndCauses: Map[FailureReason, List[Eval]] =
    failures.m.mapValues(_.flatten)

  def rootCauses: Set[Name] =
    rootCausesAndInputs.keySet

  def rootCausesAndInputs: Map[Name, Set[FailureReason]] = {
    type R = Multimap[String, Set, FailureReason]
    def loope(e: Eval, r: R): R =
      e.reasonsAndCauses.foldLeft(r)((q, kv) => loopk(e.name, kv._1, kv._2, q))
    def loopk(k: Name, f: FailureReason, vs: List[Eval], r: R): R =
      if (vs.isEmpty)
        r.add(k.value, f)
      else
        vs.foldLeft(r)((q, v) => loope(v, q))
    loope(this, Multimap.empty).m.toList.map(x => (Value(x._1), x._2)).toMap
  }

  def failureTree = failureTreeI("")
  def failureTreeI(indent: String): String = Util.quickSB(failureTreeSB(_, indent))
  def failureTreeSB(sb: StringBuilder, indent: String): Unit =
    Util.asciiTreeSB[Eval](List(this))(sb,
      _.reasonsAndCauses.valuesIterator.flatMap(_.toList).map(v => (v.name.value, v)).toMap.toList.sortBy(_._1).map(_._2),
      _.name.value, indent)

  def rootCauseTree = rootCauseTreeI("")
  def rootCauseTreeI(indent: String): String = Util.quickSB(rootCauseTreeSB(_, indent))
  def rootCauseTreeSB(sb: StringBuilder, indent: String): Unit = {
    val m = rootCausesAndInputs
    trait X
    case class K(k: Name) extends X {
      override val toString = k.value
    }
    case class I(i: FailureReason) extends X {
      override val toString = (i: String)
    }
    case object T extends X {
      override def toString = s"${m.size} failed axioms, ${m.values.foldLeft(Set.empty[FailureReason])(_ ++ _).size} causes of failure."
    }
    val keys = m.keys.toList.map(K).sortBy(_.toString)
    Util.asciiTreeSB[X](List(T))(sb, {
        case T    => keys
        case K(k) => m(k).map(I).toList.sortBy(_.toString)
        case I(_) => Nil
      }, _.toString, indent)
  }

  def report: String = {
    val sb = new StringBuilder
    sb append "Property ["
    sb append name.value
    sb append "] "
    if (success)
      sb append "passed."
    else {
      sb append "failed.\n\nInput: ["
      sb append input.show
      sb append "]."
      sb append "\n\nRoot causes:\n"
      rootCauseTreeSB(sb, "  ")
      sb append "\n\nFailure tree:\n"
      failureTreeSB(sb, "  ")
    }
    sb.toString()
  }

  @elidable(elidable.ASSERTION)
  def assertSuccess(): Unit =
    if (failure) {
      val err = report
      val sep = "=" * 120
      System.err.println(sep)
      System.err.println(err)
      System.err.println(sep)
      throw new java.lang.AssertionError(err)
    }
}
