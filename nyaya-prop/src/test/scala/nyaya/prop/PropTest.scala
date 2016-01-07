package nyaya.prop

import scalaz.Equal
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import utest._

object PropTest extends TestSuite {

  case class Tst(run: Any => Eval => Unit) {
    def >>(next: Tst): Tst =
      Tst(a => e => {
        run(a)(e)
        next.run(a)(e)
      })
    def <<(prev: Tst) = prev >> this
  }

  def assertContains(actual: String, substr: String): Unit =
    if (!actual.contains(substr)) {
      println(s"\nExpected to find [$substr] in [$actual]\n")
      assert(false)
    }

  val sep = "="*120
  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEq(None, actual, expect)

  def assertEq[A: Equal](name: String, actual: A, expect: A): Unit =
    assertEq(Some(name), actual, expect)

  def assertEq[A: Equal](name: Option[String], actual: A, expect: A): Unit =
    if (actual ≠ expect) {
      println()
      name.foreach(n => println(s">>>>>>> $n"))
      val as = actual.toString
      val es = expect.toString
      if ((as + es) contains "\n")
        println(s"actual: ↙[\n$as]\nexpect: ↙[\n$es]")
      else
        println(s"actual: [$as]\nexpect: [$es]")
      println()
      assert(false)
    }

  val anyEq = Equal.equalA[Any]

  val DEBUG                                        = Tst(_ => v => println(s"$sep\n${v.report}\n"))
  val nop                                          = Tst(_ => _ => ())
  val ok                                           = Tst(_ => v => assert(v.success))
  val ko                                           = Tst(_ => v => assert(v.failure))
  def name         (e: String)                     = Tst(_ => v => assertEq(v.name.value, e))
  def nameOf       (p: Prop[Int])                  = name(p(0).name.value)
  val inputA                                       = Tst(e => v => assertEq(v.input.a, e)(anyEq))
  def causes       (f: Any => List[Eval] => Unit)  = Tst(i => v => f(i)(v.reasonsAndCauses.values.toList.flatten))
  val rootCause                                    = causes(_ => c => assert(c.isEmpty))
  def cause1       (f: Tst = nop)                  = causes{i => c => assert(c.size == 1); f.run(i)(c.head) }
  def causeNames   (e: String*)                    = causes(_ => c => assertEq(c.map(_.name.value).sorted, e.toList.sorted))
  def rootCauses   (f: Any => Set[String] => Unit) = Tst(i => v => f(i)(v.rootCauses.map(_.value)))
  def rootCausesN  (s: String*)                    = rootCauses(_ => v => assertEq(v.toList.sorted, s.toList.sorted))
  def rootCausesP  (p: Prop[Int]*)                 = rootCausesN(p.map(_(0).name.value): _*)
  def failureTree  (f: String => Unit)             = Tst(i => v => f(v.failureTree))
  def failureTreeIs(e: String)                     = failureTree(a => assertEq(a, e))
  def report       (f: String => Unit)             = Tst(i => v => f(v.report))
  def reportIs     (e: String)                     = report(a => assertEq(a, e))
  def reportHas    (e: String)                     = report(a => assertContains(a, e))
  def failSimple   (n: String)                     = ko >> name(n) >> inputA
  def failRoot     (n: String)                     = failSimple(n) >> rootCause

  val all = nop

  def test[A](p: Prop[A], a: A, t: Tst): Unit = {
    val result = p(a)
    (t << all).run(a)(result)
  }

  def testRootCauses[A](p: Prop[A], a: A, e: String*): Unit = {
    val r = p(a).rootCauses.toList
    val actual = r.map(_.toString).sorted
    val expect = e.map(_.toString).sorted
    assert(actual == expect)
  }

  // ===================================================================================================================

  val evenN = "even"
  val oddN  = "¬even"
  val mod3N = "mod3"
  val mod5N = "mod5"
  val upperN = "upper"

  val even = Prop.test[Int](evenN, _ % 2 == 0)
  val mod3 = Prop.test[Int](mod3N, _ % 3 == 0)
  val mod5 = Prop.test[Int](mod5N, _ % 5 == 0)
  val odd = ~even
  val mod235d = even | mod3 | mod5
  val mod235c = even & mod3 & mod5
  val upper = Prop.test[String](upperN, s => s == s.toUpperCase)

  val evenF = failSimple(evenN) >> rootCause
  val oddF  = failSimple(oddN) >> rootCause // >> cause1(evenF)
  val mod3F = failSimple(mod3N) >> rootCause
  val mod5F = failSimple(mod5N) >> rootCause
  val disF  = failSimple("(even ∨ mod3 ∨ mod5)")
  val conF  = failSimple("(even ∧ mod3 ∧ mod5)")

  override def tests = TestSuite {
    'atom {
      test(even, 2, ok)
      test(even, 3, evenF)
    }
    'negation {
      test(odd, 3, ok)
      test(odd, 2, oddF)
    }
    'doubleNegation {
      val p  = ~(~even)
      test(p, 2, ok)
      test(p, 3, evenF)
    }
    'disjunction {
      test(mod235d, 30, ok)
      test(mod235d,  4, ok)
      test(mod235d, 31, disF >> causeNames(evenN, mod3N, mod5N))
    }
   'conjunction {
     test(mod235c, 30, ok)
     test(mod235c, 31, conF >> causeNames(evenN, mod3N, mod5N))
     test(mod235c,  4, conF >> causeNames(mod3N, mod5N))
     test(mod235c, 15, conF >> cause1(evenF))
   }
   'implication {
     val mod5impEven = mod5 ==> even
     test(mod5impEven, 1, ok)
     test(mod5impEven, 10, ok)
     test(mod5impEven, 5, failSimple(s"$mod5N ⇒ $evenN") >> cause1(evenF))
   }
   'reduction {
     val evenRedMod5 = even <== mod5
     test(evenRedMod5,  1, ok)
     test(evenRedMod5, 10, ok)
     test(evenRedMod5,  5, failSimple(s"$evenN ⇐ $mod5N") >> cause1(evenF))
   }
   'biconditional {
     val evenIffMod5 = even <==> mod5
     val f = failSimple(s"$evenN ⇔ $mod5N")
     test(evenIffMod5, 10, ok)
     test(evenIffMod5,  5, f >> cause1(evenF))
     test(evenIffMod5,  2, f >> cause1(mod5F))
   }
   'nested {
     val a = even ∧ odd
     val b = even ==> a
     val c = mod5 ∧ b
     val p = mod5 ∧ c
     test(p, 10, ko >> inputA >>
       cause1(nameOf(c) >>
         cause1(nameOf(b) >>
           cause1(nameOf(a) >>
             cause1(name(oddN) >>
               rootCause)))))
   }
   'rootCause {
     test(mod235c, 10, rootCausesN(mod3N))
     val p = mod5 ∧ (even ==> (even ∧ odd)) ∧ (mod3 ∧ ~even)
     test(p, 10, rootCausesN(oddN, mod3N))
   }
   'contramap {
     case class Yay(s: String, i: Int)
     val p = mod235c.contramap[Yay](_.i) ∧ upper.contramap[Yay](_.s)
     test(p, Yay("GOOD", 30), rootCausesN() >> ok)
     test(p, Yay("Bad", 30),  rootCausesN(upperN))
     test(p, Yay("GOOD", 15), rootCausesN(evenN))
     test(p, Yay("both", 4),  rootCausesN(mod3N, mod5N, upperN))
   }
    'renamed {
      test(mod235c.rename("whateverness"), 6, failureTreeIs("whateverness\n└─ mod5"))
    }
    'forall {
      * -{
        val allEven = even.forallF[List]
        test(allEven, List(4,6), ok)
        test(allEven, Nil, ok)
        test(allEven, List(4,5,6,7), ko >> rootCausesN(evenN) >> failureTreeIs("∀{even}\n└─ even"))
      }
      * -{
      val p = mod235c.forallF[List]
      test(p, List(15, 30, 60, 25), ko >> inputA >> rootCausesN(evenN, mod3N) >> reportIs(
        """
          |Property [∀{(even ∧ mod3 ∧ mod5)}] failed.
          |
          |Input: [List(15, 30, 60, 25)].
          |
          |Root causes:
          |  2 failed axioms, 2 causes of failure.
          |  ├─ even
          |  │  ├─ Invalid input [15]
          |  │  └─ Invalid input [25]
          |  └─ mod3
          |     └─ Invalid input [25]
          |
          |Failure tree:
          |  ∀{(even ∧ mod3 ∧ mod5)}
          |  └─ (even ∧ mod3 ∧ mod5)
          |     ├─ even
          |     └─ mod3
        """.stripMargin.trim))
      }
    }
    'whitelist {
      val p = Prop.whitelist[List[Int]]("whitelist!")(_ => Set(1,2,3,7), identity)
      test(p, Nil, ok)
      test(p, List(1,1,2), ok)
      test(p, List(1,1,2,8,8,9), ko >> reportHas(
        """
          |Root causes:
          |  1 failed axioms, 1 causes of failure.
          |  └─ whitelist!
          |     └─ List(1, 1, 2, 8, 8, 9)
          |        Whitelist: (4) Set(1, 2, 3, 7)
          |        Found    : (6) List(1, 1, 2, 8, 8, 9)
          |        Illegal  : {8, 9}
        """.stripMargin.trim))
    }
    'blacklist {
      val p = Prop.blacklist[List[Int]]("blacklist!")(_ => Set(9,5,6), identity)
      test(p, Nil, ok)
      test(p, List(1,1,2), ok)
      test(p, List(5,5,2,8,8,9), ko >> reportHas(
        """
          |Root causes:
          |  1 failed axioms, 1 causes of failure.
          |  └─ blacklist!
          |     └─ List(5, 5, 2, 8, 8, 9)
          |        Blacklist: (3) Set(9, 5, 6)
          |        Found    : (6) List(5, 5, 2, 8, 8, 9)
          |        Illegal  : {5, 9}
        """.stripMargin.trim))
    }
    'allPresent {
      val p = Prop.allPresent[List[Int]]("allPresent!")(_ => Set(1,2,3,0), identity)
      test(p, List(1,1,2,5), ko >> reportHas(
        """
          |Root causes:
          |  1 failed axioms, 1 causes of failure.
          |  └─ allPresent!
          |     └─ List(1, 1, 2, 5)
          |        Required: (4) Set(1, 2, 3, 0)
          |        Found   : (4) List(1, 1, 2, 5)
          |        Missing : {0, 3}
        """.stripMargin.trim))
      test(p, Nil, ko)
      test(p, List(1,3,2,0), ok)
      test(p, List(1,3,2,0,6,7,8), ok)
    }
    'whitelistI {
      type T = (Set[Int], List[String])
      val p = Prop.whitelist[T]("whitelist I")(_._1, _._2.iterator.map(_.length))
      test(p, (Set(1, 3), List("a", "b", "abc", "c")), ok)
      test(p, (Set(1, 3), List("a", "bx", "abc", "c")), ko >> reportHas(
        """
          |Root causes:
          |  1 failed axioms, 1 causes of failure.
          |  └─ whitelist I
          |     └─ (Set(1, 3),List(a, bx, abc, c))
          |        Whitelist: (2) Set(1, 3)
          |        Found    : (4) Stream(1, 2, 3, 1)
          |        Illegal  : {2}
        """.stripMargin.trim))
    }
    'distinct {
      val p = Prop.distinctC[List, Char]("hello")
      test(p, List.empty, ok)
      test(p, List('a'), ok)
      test(p, List('a', 'c', 'e'), ok)
      test(p, List('a', 'c', 'a'), ko >> reportHas(
        """
          |Root causes:
          |  1 failed axioms, 1 causes of failure.
          |  └─ each hello is unique
          |     └─ Inputs: List(a, c, a)
          |        Dups: {a → 2}
        """.stripMargin.trim))
    }
  }
}
