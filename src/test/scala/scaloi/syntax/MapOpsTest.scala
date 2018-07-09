package scaloi
package syntax

import org.scalatest._
import scaloi.test.ScaloiTest

class MapOpsTest
  extends FlatSpec
     with Matchers
     with prop.Checkers
     with ScaloiTest
{

  behaviour of "MapOps"

  import MapOps._

  it should "filterKeys" in {
    check {
      (map: Map[Int, String]) =>
        val std = map.filterKeys(_ > 0)
        val loi = map.filterKeysEagerly(_ > 0)

        std === loi
    }
  }

  it should "filterValues" in {
    check {
      (map: Map[String, Long]) =>
        val loi = map.filterValues(_ > 0)

        loi.keySet.map(loi).forall(_ > 0)
    }
  }

  it should "mapValues" in {
    check {
      (map: Map[String, String]) =>
        val std = map.mapValues(Symbol.apply)
        val loi = map.mapValuesEagerly(Symbol.apply)

        std === loi
    }
  }

  it should "getOrZero" in {
    import scalaz.std.anyVal._

    val map = Map[Int, Int](1 -> 2, 3 -> 4)

    map.getOrZero(1) should be (2)
    map.getOrZero(5) should be (0)
  }


  it should "default to zero" in {
    import scalaz.std.anyVal._
    val map = Map("yes" -> 1).withDefaultZero
    map("no") should equal(0)
    map.get("no") should equal(None)
    map("yes") should equal(1)
  }

  it should "update" in {
    check {
      (map: Map[String, String], fn: String => Option[String], s: String) =>
        map.headOption match {
          case None    => map.update(s)(fn) === map
          case Some((k, v)) =>
            val next = map.update(k)(fn)
            next.get(k) === fn(v) && (map - k) === (next - k)
        }
    }
  }

  it should "raze" in {
    val map = Map[String, Int]("a" -> 1, "b" -> 0, "c" -> 3)
    map.raze.toList.sorted should equal (List("a", "c", "c", "c"))
  }

  it should "adjust" in {
    val map = Map[String, Int]("a" -> 1, "b" -> 0)
    map.adjust("a")(_ + 1) should equal (Map("a" -> 2, "b" -> 0))
    map.adjust("c")(_ - 1) should equal (map)
  }
}
