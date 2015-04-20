package japgolly.nyaya

import scalaz.{Need, NonEmptyList, Contravariant}
import scalaz.syntax.foldable1._

final case class Named        [P[_], A   ](n: Name, l: Logic[P, A])        extends Logic[P, A]
final case class Mapped       [P[_], A, B](m: A => B, l: Logic[P, B])      extends Logic[P, A]
final case class Negation     [P[_], A   ](l: Logic[P, A])                 extends Logic[P, A]
final case class Conjunction  [P[_], A   ](ls: NonEmptyList[Logic[P, A]])  extends Logic[P, A]
final case class Disjunction  [P[_], A   ](ls: NonEmptyList[Logic[P, A]])  extends Logic[P, A]
final case class Implication  [P[_], A   ](a: Logic[P, A], c: Logic[P, A]) extends Logic[P, A]
final case class Reduction    [P[_], A   ](c: Logic[P, A], a: Logic[P, A]) extends Logic[P, A]
final case class Biconditional[P[_], A   ](p: Logic[P, A], q: Logic[P, A]) extends Logic[P, A]
final case class Atom         [P[_], A   ](n: Option[Name], f: P[A])       extends Logic[P, A] {
  override def toString = n.fold(s"Atom($f)")(_.value)
}

object Logic {
  private[nyaya] def evalChildren[P[_], A](x: P[A] => Eval, ls: NonEmptyList[Logic[P, A]])
                                          (op: String, fr: FailureReason, t: Stream[Eval] => List[Eval])
                                          (implicit F: Contravariant[P]): Eval = {
    def wrap(s: String): String = if (s.indexOf(' ') >= 0) s"[$s]" else s
    val es  = ls.map(_ run x)
    val i   = es.head.input
    val ess = es.toStream
    val n   = Need(ess.reverse.map(x => wrap(x.name.value)).mkString("(", op, ")"))
    val fs = t(ess)
    if (fs.isEmpty)
      Eval.success(n, i)
    else
      Eval(n, i, Eval.root.add(fr, fs))
  }

  private[nyaya] def bibool[P[_], A](x: P[A] => Eval, lp: Logic[P, A], lq: Logic[P, A])
                                   (name: (String, String) => String, t: (Boolean, Boolean) => Boolean, fr: FailureReason)
                                   (implicit F: Contravariant[P]): Eval = {
    val p = lp.run(x)
    val q = lq.run(x)
    val n = Need(name(p.name.value, q.name.value))
    val i = {
      val a = p.input
      val b = q.input
      if ((a eq b) || (a.show == b.show)) p.input
      else Input((a.show, b.show))
    }
    if (t(p.success, q.success))
      Eval.success(n, i)
    else
      Eval(n, i, Eval.root.add(fr, List(p, q).filter(_.failure)))
  }
}

sealed abstract class Logic[P[_], A] {
          final def ==>          (c: Logic[P, A]): Logic[P, A] = Implication(this, c)
          final def <==          (a: Logic[P, A]): Logic[P, A] = Reduction(this, a)
          final def <==>         (q: Logic[P, A]): Logic[P, A] = Biconditional(this, q)
  @inline final def |            (q: Logic[P, A]): Logic[P, A] = this ∨ q
  @inline final def &            (q: Logic[P, A]): Logic[P, A] = this ∧ q
  @inline final def ⇐            (a: Logic[P, A]): Logic[P, A] = this <== a
  @inline final def ⇔            (q: Logic[P, A]): Logic[P, A] = this <==> q
  @inline final def iff          (q: Logic[P, A]): Logic[P, A] = this <==> q
  @inline final def or           (q: Logic[P, A]): Logic[P, A] = this | q
  @inline final def and          (q: Logic[P, A]): Logic[P, A] = this & q
  @inline final def implies      (c: Logic[P, A]): Logic[P, A] = this ==> c
  @inline final def not                          : Logic[P, A] = ~this
  @inline final def subst[B <: A]                : Logic[P, B] = contramap(a => a: B)
  @inline final def rename_:(name: => String)    : Logic[P, A] = this rename name

  final def ifelse(ifPass: Logic[P, A], ifFail: Logic[P, A]): Logic[P, A] =
    (this ==> ifPass) ∧ (~this ==> ifFail)

  final def ∨(q: Logic[P, A]): Logic[P, A] = this match {
    case Disjunction(ls)        => Disjunction(q <:: ls)
    case Atom(_, _)
         | Named(_, _)
         | Mapped(_, _)
         | Negation(_)
         | Conjunction(_)
         | Implication(_, _)
         | Reduction(_, _)
         | Biconditional(_, _) => Disjunction(NonEmptyList(q, this))
  }

  final def ∧(q: Logic[P, A]): Logic[P, A] = this match {
    case Conjunction(ls)        => Conjunction(q <:: ls)
    case Atom(_, _)
         | Named(_, _)
         | Mapped(_, _)
         | Negation(_)
         | Disjunction(_)
         | Implication(_, _)
         | Reduction(_, _)
         | Biconditional(_, _) => Conjunction(NonEmptyList(q, this))
  }

  final def contramap[B](f: B => A): Logic[P, B] = this match {
    case Mapped(m, l)        => Mapped(m compose f, l)
    case Negation(l)         => Negation(l contramap f)
    case Atom(_, _)
       | Named(_, _)
       | Disjunction(_) | Conjunction(_) // https://github.com/japgolly/nyaya/issues/11
       | Implication(_, _)
       | Reduction(_, _)
       | Biconditional(_, _) => Mapped(f, this)
  }

  final def unary_~ : Logic[P, A] = this match {
    case Negation(l)                   => l
    case Named(n, l)                   => Named(n, ~l)
    case Mapped(m, l)                  => Mapped(m, ~l)
    case Disjunction(ls)               => Conjunction(ls.map(~_))
    case Conjunction(ls)               => Disjunction(ls.map(~_))
    case Biconditional(Negation(p), q) => p ⇔ q
    case Biconditional(p, q)           => p ⇔ ~q
    case Atom(_, _)
      | Implication(_, _)
      | Reduction(_, _)                => Negation(this)
    //case Implication(a, c)           => a ∧ ~c
  }

  final def rename(n: => String): Logic[P, A] = this match {
    case Named(_, l)         => Named(Need(n), l)
    case Mapped(m, l)        => Mapped(m, l rename n)
    case Atom(_, _)
       | Negation(_)
       | Conjunction(_)
       | Disjunction(_)
       | Implication(_, _)
       | Reduction(_, _)
       | Biconditional(_, _) => Named(Need(n), this)
  }

  final def run(x: P[A] => Eval)(implicit F: Contravariant[P]): Eval = this match {
    case Atom(_, fa)         => x(fa)
    case Named(n, l)         => l.run(x).rename(n)
    case Mapped(m, l)        => l.run(fb => x(F.contramap(fb)(m)))
    case Implication(a, c)   => Logic.bibool(x, a, c)(_ + " ⇒ "+ _, !_ || _ , "Implication failed.")
    case Reduction(c, a)     => Logic.bibool(x, c, a)(_ + " ⇐ "+ _, _  || !_, "Reduction failed.")
    case Biconditional(p, q) => Logic.bibool(x, p, q)(_ + " ⇔ "+ _, _  == _ , "Biconditional failed.")

    case Negation(l) =>
      val e = l.run(x)
      val n = Need("¬" + e.name.value)
      val f = if (e.failure) Eval.root
              else           Eval.root.add(s"Failure expected with input [${e.input.show}].", Nil)
      Eval(n, e.input, f)

    case Conjunction(ls) =>
      Logic.evalChildren(x, ls)(" ∧ ", "Conjuncts failed.", _.filter(_.failure).toList)

    case Disjunction(ls) =>
      Logic.evalChildren(x, ls)(" ∨ ", "Disjuncts all failed.", e => if (e.exists(_.success)) Nil else e.toList)
  }
}
