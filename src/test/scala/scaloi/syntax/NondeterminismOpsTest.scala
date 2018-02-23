package scaloi.syntax

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scalaz.Nondeterminism
import scalaz.concurrent.Task

class NondeterminismOpsTest extends FunSuite with Checkers {
  import NondeterminismOps._

  val shortSleep = Gen.choose(0L, 100L)
  val longSleep  = Gen.choose(1000L, 1100L)

  test("onLeft runs before right ends if right is slow") {
    check(sleepTest(shortSleep, longSleep, shortSleep, shortSleep, shortSleep)((l, r, a, b, c) => a.start < r.end))
  }

  test("onRight runs before left ends if left is slow") {
    check(sleepTest(longSleep, shortSleep, shortSleep, shortSleep, shortSleep)((l,r,a,b,c) => b.start < l.end))
  }

  test("onBoth runs before onLeft ends if onLeft is slow") {
    check(sleepTest(shortSleep, shortSleep, longSleep, shortSleep, shortSleep)((l,r,a,b,c) => c.start < a.end))
  }

  test("onBoth runs before onRight ends if onRight is slow") {
    check(sleepTest(shortSleep, shortSleep, shortSleep, longSleep, shortSleep)((l,r,a,b,c) => c.start < b.end))
  }

  def sleepFor(millis: Long) = Task {
    val start = System.currentTimeMillis()
    Thread.sleep(millis)
    val end = System.currentTimeMillis()
    TimedRun(start, end, s"Sleep for $millis")
  }

  def sleepTest(lGen: Gen[Long], rGen: Gen[Long], aGen: Gen[Long], bGen: Gen[Long], cGen: Gen[Long])(
      f: (TimedRun, TimedRun, TimedRun, TimedRun, TimedRun) => Boolean): Prop = {
    forAll(lGen, rGen, aGen, bGen, cGen)({ (lSleep, rSleep, aSleep, bSleep, cSleep) =>
      val N: Nondeterminism[Task] = Nondeterminism[Task]
      val joined = N.flatWye(sleepFor(lSleep), sleepFor(rSleep))(l => sleepFor(aSleep).map(a => (l, a)),
                                                                 r => sleepFor(bSleep).map(b => (r, b)),
                                                                 (_, _) => sleepFor(cSleep))
      val ((l, a), (r, b), c) = joined.unsafePerformSync

      f(l, r, a, b, c)
    })
  }
}

case class TimedRun(start: Long, end: Long, name: String)
object TimedRun {

  def graph(benchMarks: List[TimedRun], width: Int = 16, active: Char = '*', sleep: Char = '-'): List[String] = {

    val startTime = benchMarks.map(_.start).min
    val endTime   = benchMarks.map(_.end).max
    val delta     = endTime - startTime

    List(s"Total time: $delta ns (${(delta.toDouble / 1000000000).formatted("%1$,.2f")} seconds)") ++ (benchMarks map {
      benchmark =>
        val lowerbound = (benchmark.start - startTime).toDouble / delta.toDouble
        val upperbound = (benchmark.end - startTime).toDouble / delta.toDouble
        val row = for {
          i <- 1 to width
        } yield {
          val percentage = i.toDouble / width.toDouble
          if (percentage >= lowerbound && percentage <= upperbound) active else sleep
        }
        val duration = benchmark.end - benchmark.start
        s"${row.mkString}: ${benchmark.name} (${(duration.toDouble / 1000000000).formatted("%1$,.2f")} s)"
    })
  }
}
