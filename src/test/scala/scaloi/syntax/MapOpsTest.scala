package scaloi
package syntax

import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

class MapOpsTest extends FlatSpec with Matchers with Checkers {

  behavior of "MapOps"

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
}
