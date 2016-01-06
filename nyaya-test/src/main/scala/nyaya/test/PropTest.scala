package nyaya.test

import nyaya.gen._
import nyaya.prop._
import nyaya.util.Util
import scala.Console._

object PropTest extends PropTestOps {
  implicit def defaultPropSettings = DefaultSettings.propSettings
}

object PropTestOps extends PropTestOps

trait PropTestOps {

  def testProp[A](p: Prop[A], g: Gen[A])(implicit S: Settings): Unit =
    assertSuccessful(p, PTest.test(p, g, S))

  def proveProp[A](p: Prop[A], d: Domain[A])(implicit S: Settings): Unit = {
    val rs = PTest.prove(p, d, S)
    assertSuccessful(p, rs)
    rs match {
      case RunState(n, Result.Satisfied) =>
        println(s"${YELLOW}Proposition $RESET[$p]$YELLOW was satisfied but not proved after $RED$n runs$YELLOW of an expected ${d.size}.")
        fail(s"Proposition [$p] was satisfied but not proved after $n runs of an expected ${d.size}.")
      case _ => ()
    }
  }

  private[this] def assertSuccessful[A](p: Prop[A], rs: RunState[A])(implicit S: Settings): Unit =
    rs match {
      case RunState(_, _: Result.Success) =>
        ()
      case RunState(runs, f: Result.Failure[A]) =>
        printPropFailureInfo(p, runs, f)
        throwPropFailure(p, f)
    }

  private[this] def printFailureInfo[A](a: A, header: String => String, printFooter: => Unit)(implicit S: Settings): Unit = {
    if (!S.debug) {
      val v = a.toString
      val w = Util.escapeString(v)
      println(header(v))
      if (w != v) println(s"$w\n")
    }
    printFooter
    println()
  }

  def printPropFailureInfo[A](p: Prop[A], runs: Int, f: Result.Failure[Any])(implicit S: Settings): Unit =
    f match {
      case e: Result.Falsified[Any] =>
        val name = e.eval.name.value
        printFailureInfo(e.a, v => s"\n${RED}Falsified $WHITE_B[$name]$RESET$RED after $runs runs with:$RESET\n$v\n",
          println(e.eval.report))

      case e: Result.Error[Any] =>
        printFailureInfo(e.a, v => s"\n${RED_B}Crashed $WHITE_B$RED[$p]$RESET$RED_B after $runs runs with:$RESET\n$v\n",
          e.error.printStackTrace())
    }

  def throwPropFailure[A](p: Prop[A], f: Result.Failure[Any]): Nothing =
    f match {
      case e: Result.Falsified[Any] => fail("Failed: " + e.eval.name.value)
      case e: Result.Error    [Any] => fail("Failed: " + p, e.error)
    }

  private[test] def fail(s: String, e: Throwable = null): Nothing =
    throw new java.lang.AssertionError(s, e)

  @inline implicit def autoToOpsPropExt  [A](p: Prop[A])   = new PropExt(p)
  @inline implicit def autoToOpsGenExt   [A](g: Gen[A])    = new GenExt(g.run)
  @inline implicit def autoToOpsDomainExt[A](d: Domain[A]) = new DomainExt(d)
}

import nyaya.test.PropTestOps._

class PropExt[A](private val p: Prop[A]) extends AnyVal {
  def mustBeSatisfiedBy         (g: Gen[A])(implicit S: Settings) = testProp(p, g)
  def mustBeSatisfiedBy_[B <: A](g: Gen[B])(implicit S: Settings) = testProp(p, g)

  def mustBeProvedBy         (d: Domain[A])(implicit S: Settings) = proveProp(p, d)
  def mustBeProvedBy_[B <: A](d: Domain[B])(implicit S: Settings) = proveProp(p, d.subst[A])
}

class GenExt[A](private val g: Gen.Run[A]) extends AnyVal {
  def mustSatisfy         (p: Prop[A])   (implicit S: Settings) = testProp(p, Gen(g))
  def mustSatisfyE        (f: A => EvalL)(implicit S: Settings) = testProp(Prop eval f, Gen(g))
  def _mustSatisfy[B >: A](p: Prop[B])   (implicit S: Settings) = testProp(p, Gen(g))

  /**
   * Find a seed that deterministically produces failure-inducing random data.
   */
  def bugHunt(seedStart     : Long    = 0L,
              seeds         : Int     = 10000,
              samplesPerSeed: Int     = 1,
              startMsg      : Boolean = true,
              printFailure  : Boolean = true,
              filterFailure : Result.Failure[A] => Boolean = _ => true)
             (p: Prop[A])(implicit S: Settings): Unit = {
    if (startMsg)
      println(s"Starting ${p.} bugHunt: $seeds x $samplesPerSeed")
    val s2 = S.setSampleSize(samplesPerSeed)
    for (n <- 0 until seeds) {
      val seed = seedStart + n
      val g2 = Gen { c =>
        c.setSeed(seed)
        g(c)
      }
      PTest.test(p, g2, s2) match {
        case RunState(_, _: Result.Success)    => ()
        case RunState(_, f: Result.Failure[A]) =>
          if (filterFailure(f)) {
            if (printFailure)
              printPropFailureInfo(p, n, f)
            println(s"$YELLOW_B${BLACK}Found issue with seed:$RESET $BOLD$YELLOW$BLACK_B$seed$RESET\n")
            throwPropFailure(p, f)
          }
      }
    }
    println(s"$YELLOW_B${BLACK}Bug hunt failed.$RESET\n")
  }
}

class DomainExt[A](private val d: Domain[A]) extends AnyVal {
  def mustProve         (p: Prop[A])   (implicit S: Settings) = proveProp(p, d)
  def mustProveE        (f: A => EvalL)(implicit S: Settings) = proveProp(Prop eval f, d)
  def _mustProve[B >: A](p: Prop[B])   (implicit S: Settings) = proveProp(p, d.subst[B])
}
