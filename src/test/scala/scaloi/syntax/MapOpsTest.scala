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

  import map._

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

  it should "combine maps" in {
    import scalaz.std.string._
    val m1 = Map[Int, String](1 -> "asdf", 2 -> "foo")
    val m2 = Map[Int, String](2 -> "bar", 3 -> "smoosh")
    m1 combine m2 should equal (Map(
      1 -> "asdf", 2 -> "foobar", 3 -> "smoosh",
    ))

    (m1 combineWith m2)((l, r) => l ++ r ++ l) should equal (Map(
      1 -> "asdf", 2 -> "foobarfoo", 3 -> "smoosh",
    ))
  }

  it should "get to disjunction" in {
    import scalaz.syntax.either._
    Map(1 -> 2).getRightDisjunction(1) shouldEqual 2.right
    Map(1 -> 2).getRightDisjunction(0) shouldEqual 0.left
    Map(1 -> 2).getRightDisjunction(1, "a") shouldEqual 2.right
    Map(1 -> 2).getRightDisjunction(0, "a") shouldEqual "a".left
  }

  it should "traverse keys" in {
    import scalaz.syntax.std.option._
    import scalaz.std.option._
    Map(1 -> 2).traverseKeys(_.some) shouldEqual Some(Map(1 -> 2))
    Map(1 -> 3, 2 -> 4).traverseKeys(n => if(n % 2 ==0) n.some else None) shouldEqual None
  }

  it should "remap" in {
    Map(1 -> 2, 2 -> 3).remap(_ + 5) shouldEqual Map(1 -> 6, 2 -> 7)
  }
}
